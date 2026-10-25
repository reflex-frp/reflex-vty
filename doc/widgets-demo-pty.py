#!/usr/bin/env python3
"""VHS-only transport for F6–F9; the demo's bindings remain unchanged."""

import errno
import fcntl
import os
import pty
import selectors
import signal
import sys
import termios
import time
import tty

TIMEOUT = 180
HIGH_WATER = 65536
KEYS = {
    0x06: b"\x1b[17~",  # Ctrl+F -> F6
    0x07: b"\x1b[18~",  # Ctrl+G -> F7
    0x0F: b"\x1b[19~",  # Ctrl+O -> F8
    0x14: b"\x1b[20~",  # Ctrl+T -> F9
}


def window_size():
    return fcntl.ioctl(0, termios.TIOCGWINSZ, bytes(8))


def terminate(pid):
    """Reap even a child that ignores SIGTERM, without an unbounded wait."""
    if os.waitpid(pid, os.WNOHANG)[0]:
        return
    os.kill(pid, signal.SIGTERM)
    deadline = time.monotonic() + 2
    while time.monotonic() < deadline:
        if os.waitpid(pid, os.WNOHANG)[0]:
            return
        time.sleep(0.02)
    os.kill(pid, signal.SIGKILL)
    os.waitpid(pid, 0)


def forward(executable):
    original_termios = termios.tcgetattr(0)
    size = window_size()
    pid, master = pty.fork()
    if pid == 0:
        try:
            fcntl.ioctl(0, termios.TIOCSWINSZ, size)
            os.execv(executable, [executable])
        except OSError as exc:
            print(f"widgets-demo-pty: exec: {exc}", file=sys.stderr)
        os._exit(127)

    status = None
    flags = {}
    old_winch = signal.getsignal(signal.SIGWINCH)
    resized = False

    def on_resize(_signum, _frame):
        nonlocal resized
        resized = True

    try:
        tty.setraw(0)
        signal.signal(signal.SIGWINCH, on_resize)
        # stdin and stdout may share an open file description.
        flags = {fd: fcntl.fcntl(fd, fcntl.F_GETFL) for fd in (0, 1, master)}
        for fd in flags:
            os.set_blocking(fd, False)
        to_child = bytearray()
        to_terminal = bytearray()
        stdin_eof = child_eof = False
        deadline = time.monotonic() + TIMEOUT
        with selectors.DefaultSelector() as selector:
            def watch(fd, events):
                try:
                    current = selector.get_key(fd)
                except KeyError:
                    if events:
                        selector.register(fd, events)
                else:
                    if not events:
                        selector.unregister(fd)
                    elif current.events != events:
                        selector.modify(fd, events)

            while True:
                if status is None:
                    waited, child_status = os.waitpid(pid, os.WNOHANG)
                    if waited:
                        status = child_status
                if child_eof and not to_terminal and status is not None:
                    code = os.waitstatus_to_exitcode(status)
                    return code if code >= 0 else 128 - code
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise TimeoutError(f"exceeded {TIMEOUT}-second recording deadline")
                if resized:
                    fcntl.ioctl(master, termios.TIOCSWINSZ, window_size())
                    resized = False
                watch(0, selectors.EVENT_READ if not stdin_eof and not child_eof
                      and len(to_child) < HIGH_WATER else 0)
                watch(1, selectors.EVENT_WRITE if to_terminal else 0)
                watch(master, (selectors.EVENT_READ if not child_eof
                               and len(to_terminal) < HIGH_WATER else 0)
                      | (selectors.EVENT_WRITE if to_child and not child_eof else 0))
                for key, events in selector.select(min(remaining, 0.1)):
                    fd = key.fd
                    if events & selectors.EVENT_READ:
                        try:
                            chunk = os.read(fd, 4096)
                        except BlockingIOError:
                            continue
                        except OSError as exc:
                            if fd != master or exc.errno != errno.EIO:
                                raise
                            chunk = b""
                        if fd == 0:
                            if chunk:
                                # Build complete sequences before forwarding, never
                                # send a separately timed Escape as a function key.
                                to_child.extend(b"".join(KEYS.get(byte, bytes((byte,)))
                                                         for byte in chunk))
                            else:
                                stdin_eof = True
                        elif chunk:
                            to_terminal.extend(chunk)
                        else:
                            child_eof = True
                            to_child.clear()
                    if events & selectors.EVENT_WRITE:
                        pending = to_terminal if fd == 1 else to_child
                        if pending:
                            try:
                                written = os.write(fd, pending)
                            except BlockingIOError:
                                continue
                            if written == 0:
                                raise OSError("zero-length terminal write")
                            del pending[:written]
    finally:
        try:
            if status is None:
                terminate(pid)
        finally:
            signal.signal(signal.SIGWINCH, old_winch)
            for fd, original in flags.items():
                fcntl.fcntl(fd, fcntl.F_SETFL, original)
            os.close(master)
            termios.tcsetattr(0, termios.TCSANOW, original_termios)


def main():
    if len(sys.argv) != 2:
        print("usage: widgets-demo-pty.py <widgets-demo-binary>", file=sys.stderr)
        return 2
    executable = os.path.abspath(sys.argv[1])
    if not os.path.isfile(executable) or not os.access(executable, os.X_OK):
        print("widgets-demo-pty: expected an executable file", file=sys.stderr)
        return 2
    try:
        return forward(executable)
    except (OSError, TimeoutError) as exc:
        print(f"widgets-demo-pty: {exc}", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        return 130


if __name__ == "__main__":
    sys.exit(main())
