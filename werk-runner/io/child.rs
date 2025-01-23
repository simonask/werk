use std::{
    future::Future,
    io,
    pin::Pin,
    task::{Context, Poll},
};

use futures::{
    io::BufReader, ready, AsyncBufRead, AsyncRead, AsyncWrite, FutureExt as _, Stream, StreamExt,
};

pub trait Child: Send + Sync + Unpin {
    fn stdin(self: Pin<&mut Self>) -> Option<Pin<&mut dyn AsyncWrite>>;
    fn stderr(self: Pin<&mut Self>) -> Option<Pin<&mut dyn AsyncRead>>;

    fn take_stdin(&mut self) -> Option<Pin<Box<dyn AsyncWrite + Send>>>;
    fn take_stdout(&mut self) -> Option<Pin<Box<dyn AsyncRead + Send>>>;
    fn take_stderr(&mut self) -> Option<Pin<Box<dyn AsyncRead + Send>>>;

    /// Wait for the process to exit. Does NOT drop the stdin handle.
    fn status(
        &mut self,
    ) -> Pin<Box<dyn Future<Output = Result<std::process::ExitStatus, std::io::Error>> + Send>>;
}

impl Child for smol::process::Child {
    fn stdin(self: Pin<&mut Self>) -> Option<Pin<&mut dyn AsyncWrite>> {
        let stdin = Pin::new(self.get_mut().stdin.as_mut()?);
        Some(stdin as _)
    }

    fn stderr(self: Pin<&mut Self>) -> Option<Pin<&mut dyn AsyncRead>> {
        let stderr = Pin::new(self.get_mut().stderr.as_mut()?);
        Some(stderr as _)
    }

    fn take_stdin(&mut self) -> Option<Pin<Box<dyn futures::AsyncWrite + Send>>> {
        self.stdin.take().map(|s| Box::pin(s) as _)
    }

    fn take_stdout(&mut self) -> Option<Pin<Box<dyn AsyncRead + Send>>> {
        self.stdout.take().map(|s| Box::pin(s) as _)
    }

    fn take_stderr(&mut self) -> Option<Pin<Box<dyn AsyncRead + Send>>> {
        self.stderr.take().map(|s| Box::pin(s) as _)
    }

    fn status(
        &mut self,
    ) -> Pin<Box<dyn Future<Output = Result<std::process::ExitStatus, std::io::Error>> + Send>>
    {
        Box::pin(self.status())
    }
}

pub enum ChildCaptureOutput {
    /// stderr was available.
    Stderr(Vec<u8>),
    /// Will only be produced when the child was spawned with `forward_stdout =
    /// true`.
    Stdout(Vec<u8>),
    Exit(std::process::ExitStatus),
}

/// Read lines of the output from a child process, capturing stderr
/// (informational output).
///
/// If both stdout and stderr are simultaneously available, both are emitted
/// simultaneously.
#[expect(clippy::type_complexity)]
pub struct ChildLinesStream {
    stdout: Option<ByteLines<BufReader<Pin<Box<dyn AsyncRead + Send>>>>>,
    stderr: Option<ByteLines<BufReader<Pin<Box<dyn AsyncRead + Send>>>>>,
    status: Pin<Box<dyn Future<Output = std::io::Result<std::process::ExitStatus>> + Send>>,
}

impl ChildLinesStream {
    pub fn new(child: &mut dyn Child, take_stdin: bool) -> ChildLinesStream {
        if take_stdin {
            _ = child.take_stdin();
        }

        let stdout = child.take_stdout().map(|stdout| {
            let reader = BufReader::new(stdout);
            ByteLines {
                reader,
                buf: Vec::new(),
                bytes: Vec::new(),
                read: 0,
            }
        });

        let stderr = child.take_stderr().map(|stderr| {
            let stderr = BufReader::new(stderr);
            ByteLines {
                reader: stderr,
                buf: Vec::new(),
                bytes: Vec::new(),
                read: 0,
            }
        });

        let status = child.status();

        ChildLinesStream {
            stdout,
            stderr,
            status,
        }
    }
}

impl Stream for ChildLinesStream {
    type Item = io::Result<ChildCaptureOutput>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let this = Pin::into_inner(self);

        // Note: We have to wake ourselves whenever we produce a result that
        // isn't that the process exited.

        if let Some(stderr) = this.stderr.as_mut() {
            match stderr.poll_next_unpin(cx)? {
                Poll::Ready(Some(line)) => {
                    // More might be available.
                    cx.waker().wake_by_ref();
                    return Poll::Ready(Some(Ok(ChildCaptureOutput::Stderr(line))));
                }
                Poll::Ready(None) => {
                    // Stop polling the reader.
                    this.stderr = None;
                }
                Poll::Pending => (),
            }
        }

        if let Some(stdout) = this.stdout.as_mut() {
            match stdout.poll_next_unpin(cx)? {
                Poll::Ready(Some(line)) => {
                    // More might be available.
                    cx.waker().wake_by_ref();
                    return Poll::Ready(Some(Ok(ChildCaptureOutput::Stdout(line))));
                }
                Poll::Ready(None) => {
                    // Stop polling the reader.
                    this.stdout = None;
                }
                Poll::Pending => (),
            }
        }

        if this.stdout.is_none() && this.stderr.is_none() {
            this.status
                .poll_unpin(cx)?
                .map(|status| Some(Ok(ChildCaptureOutput::Exit(status))))
        } else {
            Poll::Pending
        }
    }
}

pin_project_lite::pin_project! {
    /// Like `futures::io::Lines`, but for byte streams - doesn't require valid UTF-8.
    struct ByteLines<R> {
        #[pin]
        reader: R,
        // TODO: It feels like only one of these buffers is necessary.
        buf: Vec<u8>,
        bytes: Vec<u8>,
        read: usize,
    }
}

impl<R: AsyncBufRead> Stream for ByteLines<R> {
    type Item = io::Result<Vec<u8>>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let this = self.project();
        let n = ready!(read_byte_line_internal(
            this.reader,
            cx,
            this.buf,
            this.bytes,
            this.read
        ))?;
        *this.read = 0;
        if n == 0 && this.buf.is_empty() {
            return Poll::Ready(None);
        }
        if this.buf.last() == Some(&b'\n') {
            this.buf.pop();
            if this.buf.last() == Some(&b'\r') {
                this.buf.pop();
            }
        }
        Poll::Ready(Some(Ok(std::mem::take(this.buf))))
    }
}

/// Adapted from: <https://docs.rs/futures-util/0.3.31/src/futures_util/io/read_line.rs.html#32>
fn read_byte_line_internal<R: AsyncBufRead + ?Sized>(
    reader: Pin<&mut R>,
    cx: &mut Context<'_>,
    buf: &mut Vec<u8>,
    bytes: &mut Vec<u8>,
    read: &mut usize,
) -> Poll<io::Result<usize>> {
    let ret = ready!(read_until_internal(reader, cx, b'\n', bytes, read));
    *read = 0;
    std::mem::swap(buf, bytes);
    Poll::Ready(ret)
}

/// <https://docs.rs/futures-util/0.3.31/src/futures_util/io/read_until.rs.html#27>
fn read_until_internal<R: AsyncBufRead + ?Sized>(
    mut reader: Pin<&mut R>,
    cx: &mut Context<'_>,
    byte: u8,
    buf: &mut Vec<u8>,
    read: &mut usize,
) -> Poll<io::Result<usize>> {
    loop {
        let (done, used) = {
            let available = ready!(reader.as_mut().poll_fill_buf(cx))?;
            if let Some(i) = memchr::memchr(byte, available) {
                buf.extend_from_slice(&available[..=i]);
                (true, i + 1)
            } else {
                buf.extend_from_slice(available);
                (false, available.len())
            }
        };
        reader.as_mut().consume(used);
        *read += used;
        if done || used == 0 {
            return Poll::Ready(Ok(*read));
        }
    }
}
