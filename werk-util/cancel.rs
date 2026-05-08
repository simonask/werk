//! Cancellation signal

use std::{
    pin::Pin,
    sync::{Arc, atomic::AtomicBool},
    task::{Context, Poll},
};

use event_listener::{Event, EventListener};

pub struct Sender {
    state: Arc<State>,
}

pin_project_lite::pin_project! {
    pub struct Receiver {
        state: Arc<State>,
        #[pin]
        listener: EventListener,
    }
}

impl Clone for Receiver {
    fn clone(&self) -> Self {
        Self {
            state: self.state.clone(),
            listener: self.state.event.listen(),
        }
    }
}

struct State {
    cancelled: AtomicBool,
    event: Event,
}

impl Sender {
    #[must_use]
    #[inline]
    pub fn new() -> Self {
        Sender {
            state: Arc::new(State {
                cancelled: AtomicBool::new(false),
                event: Event::new(),
            }),
        }
    }

    #[inline]
    #[must_use]
    pub fn receiver(&self) -> Receiver {
        Receiver {
            state: self.state.clone(),
            listener: self.state.event.listen(),
        }
    }

    #[inline]
    pub fn forget(self) {
        // Drop the sender, but keep the receiver alive.
        std::mem::forget(self);
    }

    #[inline]
    pub fn cancel(&self) {
        if self
            .state
            .cancelled
            .swap(true, std::sync::atomic::Ordering::SeqCst)
        {
            return;
        }
        self.state.event.notify(usize::MAX);
    }
}

impl Default for Sender {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Sender {
    fn drop(&mut self) {
        // Cancel the receiver if the sender is dropped.
        self.cancel();
    }
}

impl Future for Receiver {
    type Output = ();

    #[inline]
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();
        if this
            .state
            .cancelled
            .load(std::sync::atomic::Ordering::SeqCst)
        {
            return Poll::Ready(());
        }

        _ = this.listener.poll(cx);
        Poll::Pending
    }
}
