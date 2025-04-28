//! Cancellation signal

use std::{
    pin::Pin,
    sync::{Arc, atomic::AtomicBool},
    task::{Context, Poll, Waker},
};

use parking_lot::Mutex;

pub struct Sender {
    state: Arc<State>,
}

#[derive(Clone)]
pub struct Receiver {
    state: Arc<State>,
}

struct State {
    cancelled: AtomicBool,
    wakers: Mutex<Vec<Waker>>,
}

impl Sender {
    #[must_use]
    #[inline]
    pub fn new() -> Self {
        Sender {
            state: Arc::new(State {
                cancelled: AtomicBool::new(false),
                wakers: Mutex::new(Vec::new()),
            }),
        }
    }

    #[inline]
    #[must_use]
    pub fn receiver(&self) -> Receiver {
        Receiver {
            state: self.state.clone(),
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

        let mut wakers = self.state.wakers.lock();
        for waker in wakers.drain(..) {
            waker.wake();
        }
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

impl Receiver {
    fn poll_pinned(&self, cx: &mut Context<'_>) -> Poll<()> {
        if self
            .state
            .cancelled
            .load(std::sync::atomic::Ordering::SeqCst)
        {
            return Poll::Ready(());
        }

        let mut wakers = self.state.wakers.lock();
        wakers.push(cx.waker().clone());
        Poll::Pending
    }
}

impl Future for Receiver {
    type Output = ();

    #[inline]
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.poll_pinned(cx)
    }
}

impl Future for &Receiver {
    type Output = ();

    #[inline]
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.poll_pinned(cx)
    }
}
