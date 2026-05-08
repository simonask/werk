use std::{
    mem::ManuallyDrop,
    pin::Pin,
    sync::{Arc, OnceLock},
    task::{Context, Poll},
};

use event_listener::{Event, EventListener};

#[derive(Clone, Copy, Debug)]
pub struct Disconnected;
impl std::fmt::Display for Disconnected {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "disconnected")
    }
}
impl std::error::Error for Disconnected {}

pub fn channel<T>() -> (Sender<T>, Receiver<T>) {
    let shared = Arc::new(Shared {
        event: Event::new(),
        data: OnceLock::new(),
    });
    (
        Sender {
            shared: ManuallyDrop::new(Arc::clone(&shared)),
        },
        Receiver { shared: shared },
    )
}

pub struct Sender<T> {
    shared: ManuallyDrop<Arc<Shared<T>>>,
}

impl<T> Sender<T> {
    pub fn send(mut self, value: T) {
        self.finish(Ok(value));
        std::mem::forget(self)
    }

    pub fn receiver(&self) -> Receiver<T> {
        Receiver {
            shared: Arc::clone(&self.shared),
        }
    }

    fn finish(&mut self, result: Result<T, Disconnected>) {
        self.shared.data.get_or_init(|| result);
        self.shared.event.notify(usize::MAX);
        unsafe {
            ManuallyDrop::drop(&mut self.shared);
        }
    }
}

impl<T> Drop for Sender<T> {
    fn drop(&mut self) {
        self.finish(Err(Disconnected));
    }
}

pub struct Receiver<T> {
    shared: Arc<Shared<T>>,
}

struct Shared<T> {
    event: Event,
    data: OnceLock<Result<T, Disconnected>>,
}

impl<T> Clone for Receiver<T> {
    fn clone(&self) -> Self {
        Self {
            shared: self.shared.clone(),
        }
    }
}

impl<T> Receiver<T> {
    pub fn recv(&self) -> RecvFut<'_, T> {
        RecvFut {
            shared: &self.shared,
            listener: self.shared.event.listen(),
        }
    }

    pub fn try_recv(&self) -> Option<Result<&T, Disconnected>> {
        self.shared.data.get().map(|result| match result {
            Ok(data) => Ok(data),
            Err(Disconnected) => Err(Disconnected),
        })
    }
}

pin_project_lite::pin_project! {
    pub struct RecvFut<'a, T> {
        shared: &'a Shared<T>,
        #[pin]
        listener: EventListener,
    }
}

impl<'a, T> Future for RecvFut<'a, T> {
    type Output = Result<&'a T, Disconnected>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        if let Some(data) = self.shared.data.get() {
            return Poll::Ready(match data {
                Ok(data) => Ok(data),
                Err(Disconnected) => Err(Disconnected),
            });
        }

        let this = self.project();
        match this.listener.poll(cx) {
            Poll::Ready(_) => match this.shared.data.get() {
                Some(Ok(data)) => Poll::Ready(Ok(data)),
                Some(Err(Disconnected)) => Poll::Ready(Err(Disconnected)),
                None => unreachable!(),
            },
            Poll::Pending => Poll::Pending,
        }
    }
}
