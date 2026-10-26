#!/usr/bin/env python3
"""VHS-only transport for the note demo recording.

The demo's own bindings are untouched.  This adapter draws a three-row caption
band below the child's viewport and selects its scene from seven control bytes.
Those bytes are stripped before the demo sees them; they are recording
controls, not application bindings.  The child PTY is three rows shorter than
the outer terminal, so the band is truly outside the demo's viewport.

The band is repainted only between complete child terminal sequences (and
complete UTF-8 characters).  Read chunks are not sequence boundaries, so the
adapter tracks escape/OSC/string/UTF-8 state before injecting anything.  A
scroll region matching the child viewport keeps child scrolling out of the
band, and no band bytes are emitted after the child leaves the alternate
screen or exits.

Every recorded action uses the demo's real bindings (``e``, ``Backspace``,
``Escape``, ``Enter``, ``q``) and ordinary typed text; there is
no key transport mapping.

Run it as ``widgets-demo-pty.py <widgets-demo-binary>`` inside the VHS tape.
The child's exit status is propagated.
"""

import errno
import fcntl
import math
import os
import pty
import selectors
import signal
import struct
import sys
import termios
import time
import tty

TIMEOUT = 180
HIGH_WATER = 65536
BAND_ROWS = 3
MIN_CHILD_ROWS = 16

# Caption scenes, keyed by the control byte the tape sends at each milestone.
# The adapter consumes these bytes, so neither the demo nor its text field
# ever sees them.  Every other byte, including Escape sequences and ordinary
# text, passes through unchanged.
SCENE_OPEN = (
    "e",
    "Open the note",
    "The saved note appears in a small dialog.",
)
SCENE_EDIT = (
    "Backspace \u00d76 \u00b7 Monday",
    "Change Friday to Monday",
    "Only the draft changes; the saved note is untouched.",
)
SCENE_CANCEL = (
    "Escape",
    "Cancel without saving",
    "The committed note still says Friday.",
)
SCENE_REOPEN = (
    "e",
    "Reopen the note",
    "The draft starts again from the saved Friday text.",
)
SCENE_RETYPE = (
    "Backspace \u00d76 \u00b7 Monday",
    "Type the new text",
    "Change the draft to Monday, then save it with Enter."
)
SCENE_SAVE = (
    "Enter",
    "Save the note",
    "The background now shows the committed Monday text.",
)
SCENE_QUIT = (
    "q",
    "Quit",
    "q closes the demo and restores the terminal.",
)

SCENES = {
    0x01: SCENE_OPEN,    # Ctrl+A: before opening the dialog
    0x02: SCENE_EDIT,    # Ctrl+B: before writing the Monday draft
    0x04: SCENE_CANCEL,  # Ctrl+D: before Escape
    0x05: SCENE_REOPEN,  # Ctrl+E: before reopening
    0x06: SCENE_RETYPE,  # Ctrl+F: before writing Monday again
    0x07: SCENE_SAVE,    # Ctrl+G: before Enter
    0x0B: SCENE_QUIT,    # Ctrl+K: before q
}

INTRO_SCENE = SCENE_OPEN

# Dracula colours from VHS's theme file: background #1e1f29, foreground
# #f8f8f2, magenta #ff79c6, plus the canonical comment colour for the rule.
BG = b"\x1b[48;2;30;31;41m"
TEXT = BG + b"\x1b[38;2;248;248;242m"
SECONDARY = BG + b"\x1b[38;2;187;187;187m"
DIM = BG + b"\x1b[38;2;98;114;164m"
SAVE_CURSOR = b"\x1b7"
RESTORE_CURSOR = b"\x1b8"

# One gentle brightening per scene, then a stable badge. No blink or reflow.
PULSE_INTERVAL = 0.05
PULSE_FRAMES = 20


def badge_style(frame):
    amount = math.sin(math.pi * min(frame, PULSE_FRAMES) / PULSE_FRAMES)
    green = round(121 + 100 * amount)
    blue = round(198 + 40 * amount)
    return f"\x1b[1;48;2;255;{green};{blue}m\x1b[38;2;30;31;41m".encode("ascii")


def window_size():
    return fcntl.ioctl(0, termios.TIOCGWINSZ, bytes(8))


def dimensions(size):
    rows, cols, _, _ = struct.unpack("HHHH", size)
    return rows, cols


def sized(rows, cols):
    return struct.pack("HHHH", rows, cols, 0, 0)


def truncate(text, width):
    if width <= 0:
        return ""
    if len(text) <= width:
        return text
    if width == 1:
        return text[:1]
    return text[: width - 1] + "\u2026"


def styled_line(row, cols, segments):
    """Render one full-width band row; the background fills every cell."""
    parts = [b"\x1b[" + str(row).encode("ascii") + b";1H", BG]
    used = 0
    for style, text in segments:
        text = truncate(text, cols - used)
        if not text:
            break
        parts.append(style)
        parts.append(text.encode("utf-8"))
        used += len(text)
    parts.append(BG)
    parts.append(b" " * (cols - used))
    return b"".join(parts)


def render_band(cols, rows, child_rows, scene, pulse_frame):
    """Three rows below the child viewport: rule, badge + title, explanation.

    The scroll region is set while the cursor is saved, so child scrolling can
    never move text into the band and the app's cursor is restored unchanged.
    """
    badge, title, explanation = scene
    top = rows - BAND_ROWS + 1
    margin = b"\x1b[1;" + str(max(1, child_rows)).encode("ascii") + b"r"
    return b"".join(
        [
            SAVE_CURSOR,
            margin,
            styled_line(top, cols, [(DIM, "\u2500" * cols)]),
            styled_line(
                top + 1,
                cols,
                [(BG, "  "), (badge_style(pulse_frame), f" {badge} "),
                 (b"\x1b[22m" + TEXT, f"  {title}")],
            ),
            styled_line(top + 2, cols, [(BG, "  "), (SECONDARY, explanation)]),
            # Leave a defined pen state for terminals whose DECRC restores only
            # the cursor, and cancel any pending autowrap from the last row.
            b"\x1b[0m",
            b"\x1b[1;1H",
            RESTORE_CURSOR,
        ]
    )


class ChildStream:
    """Track whether child output ended between complete terminal sequences."""

    GROUND = 0
    ESC = 1
    CSI = 2
    OSC = 3
    OSC_ESC = 4
    STRING = 5
    STRING_ESC = 6
    UTF8 = 7
    ESC_INTERMEDIATE = 8

    def __init__(self):
        self.state = self.GROUND
        self.utf8 = 0
        self.csi = bytearray()
        self.alt = False
        self.done = False

    def ground(self):
        return self.state == self.GROUND

    def feed(self, data):
        for byte in data:
            self._byte(byte)
        return self.done

    def _byte(self, byte):
        state = self.state
        if state == self.UTF8:
            if 0x80 <= byte <= 0xBF:
                self.utf8 -= 1
                if self.utf8 == 0:
                    self.state = self.GROUND
                return
            # Not a continuation byte: abandon the partial character and
            # interpret this byte on its own.
            self.utf8 = 0
            self.state = self.GROUND

        if self.state == self.GROUND:
            if byte == 0x1B:
                self.state = self.ESC
            elif 0xC2 <= byte <= 0xDF:
                self.utf8 = 1
                self.state = self.UTF8
            elif 0xE0 <= byte <= 0xEF:
                self.utf8 = 2
                self.state = self.UTF8
            elif 0xF0 <= byte <= 0xF4:
                self.utf8 = 3
                self.state = self.UTF8
        elif self.state == self.ESC:
            if byte == 0x1B:
                self.state = self.ESC
            elif byte == ord("["):
                self.csi.clear()
                self.state = self.CSI
            elif byte == ord("]"):
                self.state = self.OSC
            elif byte in (ord("P"), ord("X"), ord("^"), ord("_")):
                self.state = self.STRING
            elif 0x20 <= byte <= 0x2F:
                # ESC intermediates such as ESC ( B are not complete yet.
                self.state = self.ESC_INTERMEDIATE
            elif 0x30 <= byte <= 0x7E:
                self.state = self.GROUND
            else:
                self.state = self.GROUND
        elif self.state == self.ESC_INTERMEDIATE:
            if byte == 0x1B:
                self.state = self.ESC
            elif 0x20 <= byte <= 0x2F:
                self.state = self.ESC_INTERMEDIATE
            elif 0x30 <= byte <= 0x7E:
                self.state = self.GROUND
            else:
                self.state = self.GROUND
        elif self.state == self.CSI:
            if byte == 0x1B:
                self.state = self.ESC
            elif 0x20 <= byte <= 0x3F:
                if len(self.csi) < 32:
                    self.csi.append(byte)
            elif 0x40 <= byte <= 0x7E:
                params = bytes(self.csi)
                if byte == ord("h") and params in (b"?1049", b"?1047", b"?47"):
                    self.alt = True
                elif byte == ord("l") and params in (
                    b"?1049",
                    b"?1047",
                    b"?47",
                ):
                    self.alt = False
                    self.done = True
                self.state = self.GROUND
            elif byte in (0x18, 0x1A):
                self.state = self.GROUND
        elif self.state == self.OSC:
            if byte == 0x07:
                self.state = self.GROUND
            elif byte == 0x1B:
                self.state = self.OSC_ESC
        elif self.state == self.OSC_ESC:
            if byte == ord("\\"):
                self.state = self.GROUND
            elif byte != 0x1B:
                self.state = self.OSC
        elif self.state == self.STRING:
            if byte == 0x1B:
                self.state = self.STRING_ESC
        elif self.state == self.STRING_ESC:
            if byte == ord("\\"):
                self.state = self.GROUND
            elif byte != 0x1B:
                self.state = self.STRING


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
    outer = window_size()
    outer_rows, outer_cols = dimensions(outer)
    band_rows = BAND_ROWS if outer_rows >= MIN_CHILD_ROWS + BAND_ROWS else 0
    child = sized(outer_rows - band_rows, outer_cols)
    pid, master = pty.fork()
    if pid == 0:
        try:
            fcntl.ioctl(0, termios.TIOCSWINSZ, child)
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
        child_stream = ChildStream()
        state = {
            "scene": INTRO_SCENE,
            "cols": outer_cols,
            "rows": outer_rows,
            "child_rows": outer_rows - band_rows,
            "exited": False,
            "pulse_start": None,
            "pulse_frame": -1,
        }

        def band_if_safe():
            if (
                band_rows
                and child_stream.alt
                and not child_stream.done
                and not state["exited"]
                and child_stream.ground()
                and status is None
            ):
                if state["pulse_start"] is None:
                    state["pulse_start"] = time.monotonic()
                frame = min(PULSE_FRAMES, int(
                    (time.monotonic() - state["pulse_start"]) / PULSE_INTERVAL))
                to_terminal.extend(
                    render_band(
                        state["cols"],
                        state["rows"],
                        state["child_rows"],
                        state["scene"],
                        frame,
                    )
                )
                state["pulse_frame"] = frame
                return True
            return False

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
                        state["exited"] = True
                if child_eof and not to_terminal and status is not None:
                    code = os.waitstatus_to_exitcode(status)
                    return code if code >= 0 else 128 - code
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise TimeoutError(f"exceeded {TIMEOUT}-second recording deadline")
                if (state["pulse_start"] is not None
                        and state["pulse_frame"] < PULSE_FRAMES
                        and len(to_terminal) < HIGH_WATER
                        and int((time.monotonic() - state["pulse_start"])
                                / PULSE_INTERVAL) > state["pulse_frame"]):
                    band_if_safe()
                if resized:
                    resized = False
                    outer = window_size()
                    outer_rows, outer_cols = dimensions(outer)
                    band_rows = (
                        BAND_ROWS
                        if outer_rows >= MIN_CHILD_ROWS + BAND_ROWS
                        else 0
                    )
                    fcntl.ioctl(
                        master,
                        termios.TIOCSWINSZ,
                        sized(outer_rows - band_rows, outer_cols),
                    )
                    state["cols"] = outer_cols
                    state["rows"] = outer_rows
                    state["child_rows"] = outer_rows - band_rows
                    band_if_safe()
                watch(0, selectors.EVENT_READ if not stdin_eof and not child_eof
                      and len(to_child) < HIGH_WATER else 0)
                watch(1, selectors.EVENT_WRITE if to_terminal else 0)
                watch(master, (selectors.EVENT_READ if not child_eof
                               and len(to_terminal) < HIGH_WATER else 0)
                      | (selectors.EVENT_WRITE if to_child and not child_eof else 0))
                for key, events in selector.select(min(remaining, PULSE_INTERVAL)):
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
                                # Caption controls are stripped; every other byte
                                # (including complete Escape sequences) is forwarded.
                                forwarded = bytearray()
                                scene_changed = False
                                for byte in chunk:
                                    scene = SCENES.get(byte)
                                    if scene is not None:
                                        state["scene"] = scene
                                        state["pulse_start"] = None
                                        state["pulse_frame"] = -1
                                        scene_changed = True
                                    else:
                                        forwarded.append(byte)
                                if forwarded:
                                    to_child.extend(forwarded)
                                if scene_changed:
                                    band_if_safe()
                            else:
                                stdin_eof = True
                        elif chunk:
                            # The child stream is parsed before the band is
                            # considered: the band is only injected when the
                            # chunk ended between terminal sequences.
                            child_stream.feed(chunk)
                            to_terminal.extend(chunk)
                            band_if_safe()
                        else:
                            child_eof = True
                            state["exited"] = True
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
