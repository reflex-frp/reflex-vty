#!/usr/bin/env python3
"""PTY tests for reflex-vty-test-driver.

This script uses only the Python standard library.
It runs the driver from argument 1 in a new PTY.
Each process has separate HOME and XDG_CONFIG_HOME directories.
REFLEX_VTY_TEST_CONTROL_FD specifies the control file descriptor.
Each control message ends with a newline.
The tests check typed and pasted Unicode text, rendered output, and resize reports.
They also check pager scrolling with arrow keys and SGR wheel events.
Exit checks cover normal shutdown, signals, exceptions, and post-build shutdown.
The tests check termios restoration and terminal cleanup sequences.
Each process has a time limit. The script waits for child termination after failures.
"""

import fcntl
import os
import pty
import selectors
import shutil
import signal
import struct
import subprocess
import sys
import tempfile
import termios
import time

READ_TIMEOUT = 15.0  # Maximum seconds for each condition.
REAP_TIMEOUT = 10.0  # Maximum seconds for child termination.

INIT_SIZE = (24, 80)  # (rows, cols)
RESIZE_SIZE = (12, 40)

CLEANUP_CURSOR_SHOW = rb"\x1b\[\?25h"
# Cleanup must disable a mouse, paste, or alternate-screen mode.
# A cursor-show sequence alone is insufficient. Vty sends ?25l at startup.
CLEANUP_MODE_DISABLE = rb"\x1b\[\?(1000|1002|1003|1006|2004|1049)l"

ECHO_TYPED = "he\u0301"      # typed: h, e, U+0301 (combining acute)
ECHO_PASTED = "ab界e\u0301"  # paste includes both wide and combining characters
ECHO_FULL = ECHO_TYPED + ECHO_PASTED


def hs_show(s):
    """Convert a Python string to the GHC `show` format for Haskell String.

    The format matches GHC 9.12.3. Printable ASCII characters do not change.
    Control characters use named escape sequences.
    Other code points use decimal escape sequences.
    A decimal escape has a "\\&" suffix if the next character is a decimal digit.
    """
    named = {
        "\\": "\\\\",
        '"': '\\"',
        "\a": "\\a",
        "\b": "\\b",
        "\f": "\\f",
        "\n": "\\n",
        "\r": "\\r",
        "\t": "\\t",
        "\v": "\\v",
    }
    out = ['"']
    after_numeric = False
    for ch in s:
        if after_numeric and ch.isdigit():
            out.append("\\&")
        after_numeric = False
        if ch in named:
            out.append(named[ch])
        elif 32 <= ord(ch) <= 126:
            out.append(ch)
        else:
            out.append("\\%d" % ord(ch))
            after_numeric = True
    out.append('"')
    return "".join(out)


class Session:
    """One driver process in a new PTY with a time limit."""

    def __init__(self, binary, mode):
        self.home = tempfile.mkdtemp(prefix="reflex-vty-pty-")
        self.dir_home = os.path.join(self.home, "home")
        self.dir_xdg = os.path.join(self.home, "xdg")
        os.mkdir(self.dir_home)
        os.mkdir(self.dir_xdg)

        self.master, self.slave = pty.openpty()
        self.set_winsize(INIT_SIZE)

        # Save the initial attributes of the slave terminal.
        self.termios_before = termios.tcgetattr(self.slave)

        ctrl_r, ctrl_w = os.pipe()
        env = dict(os.environ)
        env.update(
            {
                "HOME": self.dir_home,
                "XDG_CONFIG_HOME": self.dir_xdg,
                "TERM": "xterm-256color",
                "LANG": "C.UTF-8",
                "LC_ALL": "C.UTF-8",
                "REFLEX_VTY_TEST_CONTROL_FD": str(ctrl_w),
            }
        )
        self.proc = subprocess.Popen(
            [binary, mode],
            stdin=self.slave,
            stdout=self.slave,
            stderr=self.slave,
            env=env,
            pass_fds=(ctrl_w,),
            start_new_session=True,
            close_fds=True,
        )
        os.close(ctrl_w)
        self.ctrl_r = ctrl_r

        self.output = b""
        self.control_buf = b""
        self.lines = []
        self.selector = selectors.DefaultSelector()
        self.selector.register(self.master, selectors.EVENT_READ, "pty")
        self.selector.register(self.ctrl_r, selectors.EVENT_READ, "ctrl")

    # -- terminal input and output ----------------------------------------

    def set_winsize(self, rows_cols):
        rows, cols = rows_cols
        winsize = struct.pack("HHHH", rows, cols, 0, 0)
        for fd in (self.slave, self.master):
            fcntl.ioctl(fd, termios.TIOCSWINSZ, winsize)

    def send(self, data):
        """Deliver input bytes to the child's terminal."""
        os.write(self.master, data)

    def sendline(self, data=b""):
        self.send(data + b"\r")

    def drain_once(self, timeout):
        for key, _ in self.selector.select(timeout):
            fd = key.fd
            try:
                chunk = os.read(fd, 65536)
            except OSError:
                chunk = b""
            if not chunk:
                self.selector.unregister(fd)
                continue
            if key.data == "pty":
                self.output += chunk
            else:
                self.control_buf += chunk
                while b"\n" in self.control_buf:
                    line, self.control_buf = self.control_buf.split(b"\n", 1)
                    self.lines.append(line.decode("utf-8", "replace"))

    def drain_available(self):
        """Read the remaining buffered bytes after the writer stops."""
        while self.selector.select(0):
            self.drain_once(0)

    def wait_for(self, pred, what, timeout=READ_TIMEOUT):
        """Read both channels until pred() is true or the time limit expires."""
        deadline = time.monotonic() + timeout
        while not pred():
            if self.proc.poll() is not None:
                # Read the remaining bytes from both channels before reporting failure.
                self.drain_available()
                if pred():
                    return
                self.fail(what + " (process exited with %r)" % self.returncode())
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                self.fail(what)
            self.drain_once(min(remaining, 0.05))

    def wait_for_line(self, prefix, timeout=READ_TIMEOUT):
        self.wait_for(
            lambda: any(line.startswith(prefix) for line in self.lines),
            "timed out waiting for control line %r" % prefix,
            timeout,
        )
        return next(line for line in self.lines if line.startswith(prefix))

    def last_value(self):
        values = [line[len("VALUE ") :] for line in self.lines if line.startswith("VALUE ")]
        return values[-1] if values else None

    def returncode(self):
        return self.proc.returncode

    def fail(self, what):
        reap(self.proc)
        raise AssertionError(
            "%s\n-- control lines --\n%s\n-- pty output --\n%s"
            % (what, "\n".join(self.lines) or "(none)", self.output.decode("utf-8", "replace"))
        )

    # -- lifecycle --------------------------------------------------------

    def shutdown(self, expect_code=0):
        """Wait for child termination within the time limit."""
        code = reap(self.proc, expect_code)
        self.drain_available()
        return code

    def close(self):
        if self.proc.returncode is None:
            reap_after_kill(self.proc)
        try:
            self.selector.close()
        except Exception:
            pass
        for fd in (self.master, self.slave, self.ctrl_r):
            try:
                os.close(fd)
            except OSError:
                pass
        shutil.rmtree(self.home, ignore_errors=True)

    def assert_termios_restored(self, context):
        now = termios.tcgetattr(self.slave)
        if now != self.termios_before:
            raise AssertionError(
                "%s: termios not restored\nbefore=%r\nafter=%r"
                % (context, self.termios_before, now)
            )

    def assert_cleanup_escapes(self, context):
        import re

        if not re.search(CLEANUP_CURSOR_SHOW, self.output):
            raise AssertionError(
                "%s: cursor not re-shown on cleanup\n%s"
                % (context, self.output.decode("utf-8", "replace"))
            )
        if not re.search(CLEANUP_MODE_DISABLE, self.output):
            raise AssertionError(
                "%s: no terminal-mode disable escape in output\n%s"
                % (context, self.output.decode("utf-8", "replace"))
            )


def reap(proc, expect_code=None):
    """Wait for child termination. Send SIGKILL if the time limit expires."""
    try:
        code = proc.wait(timeout=REAP_TIMEOUT)
    except subprocess.TimeoutExpired:
        proc.kill()
        code = proc.wait(timeout=REAP_TIMEOUT)
        if expect_code is not None:
            raise AssertionError(
                "had to SIGKILL after %ss; exit code %r, expected %r"
                % (REAP_TIMEOUT, code, expect_code)
            )
    if expect_code is not None and code != expect_code:
        raise AssertionError("exit code %r, expected %r" % (code, expect_code))
    return code


def reap_after_kill(proc):
    try:
        reap(proc)
    except Exception:
        proc.kill()
        proc.wait(timeout=REAP_TIMEOUT)
        raise


# -- tests ----------------------------------------------------------------


def test_echo_and_resize(driver_bin):
    """Check typed text, pasted text, rendered output, and terminal resize."""
    s = Session(driver_bin, "echo")
    try:
        s.wait_for_line("READY")
        s.wait_for_line("VIEWPORT 80 24")

        # Resize the terminal. The root viewport must report the new size.
        s.set_winsize(RESIZE_SIZE)
        os.kill(s.proc.pid, signal.SIGWINCH)
        s.wait_for_line("VIEWPORT 40 12")

        # Type the base character before its combining character.
        s.send(ECHO_TYPED.encode("utf-8"))
        s.wait_for(
            lambda: s.last_value() == hs_show(ECHO_TYPED),
            "typed value not reported as %s" % hs_show(ECHO_TYPED),
        )

        # Add pasted text with a wide character after the typed text.
        s.send(b"\x1b[200~" + ECHO_PASTED.encode("utf-8") + b"\x1b[201~")
        s.wait_for(
            lambda: s.last_value() == hs_show(ECHO_FULL),
            "pasted value not reported as %s" % hs_show(ECHO_FULL),
        )

        # The terminal output must contain the typed and pasted text in UTF-8.
        # This includes the base character and its combining character.
        s.wait_for(
            lambda: ECHO_FULL.encode("utf-8") in s.output,
            "terminal did not render typed and pasted Unicode text",
        )

        s.send(b"\x03")  # Ctrl-C stops the echo widget.
        s.wait_for_line("EXIT")
        s.shutdown(expect_code=0)
        s.assert_termios_restored("echo normal exit")
        s.assert_cleanup_escapes("echo normal exit")
    finally:
        reap_after_kill(s.proc)
        s.close()


def test_pager(driver_bin):
    """Arrow keys and SGR wheel events change the reported visible range."""
    s = Session(driver_bin, "pager")
    try:
        s.wait_for_line("READY")
        if s.wait_for_line("SCROLL") != "SCROLL 0 24":
            raise AssertionError("initial scroll %r" % [l for l in s.lines if l.startswith("SCROLL")])

        s.send(b"\x1b[B")  # down arrow
        s.wait_for_line("SCROLL 1 ")
        s.send(b"\x1b[<65;10;10M")  # SGR wheel down
        s.wait_for_line("SCROLL 2 ")
        s.send(b"\x1b[<64;10;10M")  # SGR wheel up
        scroll_lines = lambda: [l for l in s.lines if l.startswith("SCROLL")]
        s.wait_for(
            lambda: len(scroll_lines()) >= 4 and scroll_lines()[-1] == "SCROLL 1 24",
            "wheel-up did not return to SCROLL 1 (lines: %r)" % scroll_lines(),
        )

        s.send(b"\x03")
        s.wait_for_line("EXIT")
        s.shutdown(expect_code=0)
        s.assert_termios_restored("pager normal exit")
        s.assert_cleanup_escapes("pager normal exit")
    finally:
        reap_after_kill(s.proc)
        s.close()


def test_sigterm(driver_bin):
    """The host restores termios and sends terminal cleanup sequences after SIGTERM."""
    s = Session(driver_bin, "echo")
    try:
        s.wait_for_line("READY")
        os.kill(s.proc.pid, signal.SIGTERM)
        code = s.shutdown(expect_code=0)
        s.assert_termios_restored("SIGTERM exit (code %r)" % code)
        s.assert_cleanup_escapes("SIGTERM exit")
    finally:
        reap_after_kill(s.proc)
        s.close()


def test_postbuild_exit(driver_bin):
    """The host stops after post-build without input."""
    s = Session(driver_bin, "postbuild-exit")
    try:
        s.wait_for_line("READY")
        s.wait_for_line("EXIT")
        s.shutdown(expect_code=0)
        s.assert_termios_restored("postbuild-exit")
        if not s.lines:
            raise AssertionError("no control lines for postbuild-exit")
    finally:
        reap_after_kill(s.proc)
        s.close()


def test_throw_after_start(driver_bin):
    """The host restores the terminal and returns a nonzero exit code after an exception."""
    s = Session(driver_bin, "throw-after-start")
    try:
        s.wait_for_line("READY")
        deadline = time.monotonic() + READ_TIMEOUT
        while s.proc.poll() is None:
            s.drain_once(min(deadline - time.monotonic(), 0.05))
            if time.monotonic() > deadline:
                s.fail("throw-after-start did not terminate")
        code = s.shutdown(expect_code=None)
        if code == 0:
            raise AssertionError("throw-after-start exited 0")
        s.assert_termios_restored("throw-after-start (code %r)" % code)
        s.assert_cleanup_escapes("throw-after-start")
    finally:
        reap_after_kill(s.proc)
        s.close()


def main():
    if len(sys.argv) != 2:
        print("usage: pty_smoke.py <absolute-path-to-reflex-vty-test-driver>", file=sys.stderr)
        return 2
    driver_bin = os.path.abspath(sys.argv[1])
    if not (os.path.isfile(driver_bin) and os.access(driver_bin, os.X_OK)):
        print("not an executable: %s" % driver_bin, file=sys.stderr)
        return 2

    tests = [
        ("echo+resize+paste", test_echo_and_resize),
        ("pager", test_pager),
        ("sigterm", test_sigterm),
        ("postbuild-exit", test_postbuild_exit),
        ("throw-after-start", test_throw_after_start),
    ]
    failures = 0
    for name, fn in tests:
        try:
            fn(driver_bin)
            print("PASS %s" % name)
        except Exception as exc:
            failures += 1
            print("FAIL %s: %s" % (name, exc))
    if failures:
        print("%d of %d PTY smoke tests failed" % (failures, len(tests)))
        return 1
    print("all %d PTY smoke tests passed" % len(tests))
    return 0


if __name__ == "__main__":
    sys.exit(main())
