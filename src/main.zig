const std = @import("std");
const Allocator = std.mem.Allocator;
const Thread = std.Thread;
const Condition = Thread.Condition;
const Mutex = Thread.Mutex;

const sane = @import("sane.zig");
const ArrayList = sane.ArrayList;
const AutoHashMap = sane.AutoHashMap;
const Queue = sane.Queue;

const builtin = @import("builtin");
const is_windows = builtin.os.tag == .windows;
const should_enable_signal_handling = !is_windows and builtin.os.tag != .wasi and builtin.os.tag != .uefi;

const logger = std.log.scoped(.zigline);

fn Wrapped(comptime T: type) type {
    return struct {
        value: std.meta.Int(.unsigned, @bitSizeOf(T)),
        const t = T;
    };
}

fn fromWrapped(value: anytype) @TypeOf(value).t {
    return @errorCast(@errorFromInt(value.value));
}

fn toWrapped(comptime T: type, value: anytype) Wrapped(T) {
    return .{ .value = @intFromError(@as(T, @errorCast(value))) };
}

pub const SystemCapabilities = switch (builtin.os.tag) {
    // FIXME: Windows' console handling is a mess, and std doesn't have
    //        the necessary bindings to emulate termios on Windows.
    //        For now, we just ignore the termios problem, which will lead to
    //        garbled input; so we just ignore all that too and set the default
    //        execution mode to "NonInteractive".
    .windows,
    .wasi,
    => struct {
        const Self = @This();
        pub const Sigaction = struct {}; // This is not implemented in Zig, and we're not about to try.
        pub const pollfd = std.posix.pollfd;

        pub const termios = struct {};
        pub const V = enum {
            EOF,
            EOL,
            ERASE,
            WERASE,
            INTR,
            KILL,
            MIN,
            QUIT,
            START,
            STOP,
            SUSP,
            TIME,
        };

        pub const default_operation_mode = Configuration.OperationMode.NonInteractive;

        pub const getStdIn = std.io.getStdIn;
        pub const getStdOut = std.io.getStdOut;
        pub const getStdErr = std.io.getStdErr;

        pub const winsize = struct {
            col: u16,
            row: u16,
        };
        const getWinsize = if (builtin.os.tag == .windows) struct {
            pub fn getWinsize(handle: anytype) !winsize {
                var ws: std.posix.winsize = undefined;
                if (std.c.ioctl(handle, std.posix.T.IOCGWINSZ, @intFromPtr(&ws)) != 0) {
                    const fd = std.posix.open("/dev/tty", .{ .ACCMODE = .RDONLY }, @as(std.posix.mode_t, 0)) catch return;
                    if (fd != -1) {
                        _ = std.c.ioctl(@intCast(fd), std.posix.T.IOCGWINSZ, @intFromPtr(&ws));
                        _ = std.posix.close(@intCast(fd));
                    } else {
                        return error.Invalid;
                    }
                }
                return winsize{ .col = ws.ws_col, .row = ws.ws_row };
            }
        } else struct {
            pub fn getWinsize(handle: anytype) !winsize {
                _ = handle;
                return winsize{ .col = 80, .row = 24 };
            }
        }.getWinsize;

        pub fn getTermios() !Self.termios {
            return .{};
        }

        pub fn setTermios(_: Self.termios) !void {}

        pub fn clearEchoAndICanon(_: *Self.termios) void {}

        pub fn getTermiosCC(_: Self.termios, cc: Self.V) u8 {
            return switch (cc) {
                // Values aren't all that important, but the source is linux.
                .EOF => 4,
                .EOL => 0,
                .ERASE => 127,
                .WERASE => 23, // ctrl('w')
                .INTR => 3,
                .KILL => 21,
                .MIN => 1,
                .QUIT => 28,
                .START => 17,
                .STOP => 19,
                .SUSP => 26,
                .TIME => 0,
            };
        }

        pub const POLL_IN = std.posix.POLL.RDNORM;

        pub fn setPollFd(p: *Self.pollfd, f: anytype) void {
            if (builtin.os.tag == .windows) {
                p.fd = @ptrCast(f);
            } else {
                p.fd = f;
            }
        }

        pub const nfds_t = std.posix.nfds_t;
        pub fn poll(fds: [*]Self.pollfd, n: nfds_t, timeout: i32) c_int {
            // std.posix.poll() has a Windows implementation but doesn't accept the second arg, only a slice.
            _ = timeout;
            fds[n - 1].revents = Self.POLL_IN;
            return 1;
        }

        const pipe = (if (is_windows) struct {
            pub fn pipe() ![2]std.posix.fd_t {
                var rd: std.os.windows.HANDLE = undefined;
                var wr: std.os.windows.HANDLE = undefined;
                var attrs: std.os.windows.SECURITY_ATTRIBUTES = undefined;
                attrs.nLength = 0;
                try std.os.windows.CreatePipe(&rd, &wr, &attrs);
                return [2]std.posix.fd_t{ @ptrCast(rd), @ptrCast(wr) };
            }
        } else struct {
            pub fn pipe() ![2]std.posix.fd_t {
                return std.posix.pipe();
            }
        }).pipe;
    },
    .uefi => struct {
        const Self = @This();
        pub const Sigaction = struct {}; // No signal handling on UEFI :)
        pub const termios = struct {}; // FIXME: Implement?
        pub const V = enum {
            EOF,
            ERASE,
            WERASE,
            KILL,
        };

        pub const pollfd = struct {
            fd: u32,
            events: i16,
            revents: i16,
        };
        pub const nfds_t = u32;

        pub const default_operation_mode = Configuration.OperationMode.NonInteractive;

        const WriterContext = struct {
            console_out: *std.os.uefi.protocol.SimpleTextOutput,
            attribute: usize,
        };

        const ReaderContext = struct {
            console_in: *std.os.uefi.protocol.SimpleTextInput,
        };

        const Writer = std.io.GenericWriter(
            WriterContext,
            error{},
            struct {
                fn write(context: WriterContext, bytes: []const u8) error{}!usize {
                    _ = context.console_out.setAttribute(context.attribute);
                    for (bytes) |c| {
                        _ = context.console_out.outputString(@ptrCast(&[2]u16{ c, 0 }));
                    }
                    return bytes.len;
                }
            }.write,
        );

        const Reader = std.io.GenericReader(
            ReaderContext,
            error{},
            struct {
                fn read(context: ReaderContext, bytes: []u8) error{}!usize {
                    var i: usize = 0;
                    const writer = getStdOut().writer();
                    var key: std.os.uefi.protocol.SimpleTextInputEx.Key.Input = undefined;
                    var status = std.os.uefi.Status.NotReady;
                    while (true) {
                        status = context.console_in.readKeyStroke(&key);
                        if (status == std.os.uefi.Status.Success) {
                            if (key.scan_code == 0) {
                                if (key.unicode_char == 0xd or key.unicode_char == 0xa) {
                                    bytes[i] = '\n';
                                    i += 1;
                                    writer.writeAll("\r\n") catch {};
                                    break;
                                }

                                i += std.unicode.utf8Encode(key.unicode_char, bytes[i..]) catch 0;
                                writer.writeAll(bytes[0..i]) catch {};
                            }
                            break;
                        }
                    }

                    while (i < bytes.len) {
                        status = context.console_in.readKeyStroke(&key);
                        if (status == std.os.uefi.Status.Success) {
                            if (key.scan_code == 0) {
                                if (key.unicode_char == 0xd or key.unicode_char == 0xa) {
                                    bytes[i] = '\n';
                                    i += 1;
                                    writer.writeAll("\r\n") catch {};
                                    break;
                                }

                                i += std.unicode.utf8Encode(key.unicode_char, bytes[i..]) catch 0;
                                writer.writeAll(bytes[0..i]) catch {};
                            }
                        } else break;
                    }
                    return i;
                }
            }.read,
        );

        const File = struct {
            _reader: ?Reader,
            _writer: ?Writer,
            handle: u0 = undefined,

            pub fn reader(self: *const @This()) Reader {
                return self._reader.?;
            }

            pub fn writer(self: *const @This()) Writer {
                return self._writer.?;
            }

            pub fn write(self: *const @This(), bytes: []const u8) !usize {
                return self.writer().write(bytes);
            }

            pub fn writeAll(self: *const @This(), bytes: []const u8) !void {
                var i: usize = 0;
                while (i < bytes.len) {
                    const n = try self.write(bytes[i..]);
                    if (n == 0) {
                        break;
                    }
                    i += n;
                }
            }

            pub fn read(self: *const @This(), bytes: []u8) !usize {
                return self.reader().read(bytes);
            }
        };

        pub fn getStdIn() File {
            return File{
                ._reader = Reader{
                    .context = .{
                        .console_in = std.os.uefi.system_table.con_in.?,
                    },
                },
                ._writer = null,
            };
        }

        pub fn getStdOut() File {
            return File{
                ._reader = null,
                ._writer = Writer{
                    .context = .{
                        .console_out = std.os.uefi.system_table.con_out.?,
                        .attribute = 0x7, // white
                    },
                },
            };
        }

        pub fn getStdErr() File {
            return File{
                ._reader = null,
                ._writer = Writer{
                    .context = .{
                        .console_out = std.os.uefi.system_table.con_out.?,
                        .attribute = 0x4, // red
                    },
                },
                .handle = undefined,
            };
        }

        pub const winsize = struct {
            col: u16,
            row: u16,
        };
        const getWinsize = struct {
            pub fn getWinsize(handle: anytype) !winsize {
                _ = handle;
                return winsize{ .col = 80, .row = 24 };
            }
        }.getWinsize;

        pub fn getTermios() !Self.termios {
            return .{};
        }

        pub fn setTermios(_: Self.termios) !void {}

        pub fn clearEchoAndICanon(_: *Self.termios) void {}

        pub fn getTermiosCC(_: Self.termios, cc: Self.V) u8 {
            return switch (cc) {
                // Values aren't all that important, but the source is linux.
                .EOF => 4,
                .ERASE => 127,
                .WERASE => 23, // ctrl('w')
                .KILL => 21,
            };
        }

        pub const POLL_IN = 0;

        pub fn setPollFd(p: *Self.pollfd, f: anytype) void {
            _ = p;
            _ = f;
        }

        pub fn poll(fds: [*]Self.pollfd, n: nfds_t, timeout: i32) c_int {
            _ = timeout;
            fds[n - 1].revents = Self.POLL_IN;
            return 1;
        }

        const pipe = struct {
            pub fn pipe() ![2]std.posix.fd_t {
                return [2]std.posix.fd_t{ 0, 0 };
            }
        }.pipe;
    },
    else => struct {
        const Self = @This();
        pub const Sigaction = std.posix.Sigaction;
        pub const pollfd = std.posix.pollfd;

        pub const termios = std.posix.termios;

        pub const V = std.posix.V;

        pub const default_operation_mode = Configuration.OperationMode.Full;

        pub const getStdIn = std.io.getStdIn;
        pub const getStdOut = std.io.getStdOut;
        pub const getStdErr = std.io.getStdErr;

        pub fn getWinsize(handle: anytype) !std.posix.winsize {
            var ws: std.posix.winsize = undefined;
            const ioctl = if (builtin.os.tag == .linux) std.os.linux.ioctl else std.c.ioctl;
            if (ioctl(handle, std.posix.T.IOCGWINSZ, @intFromPtr(&ws)) != 0) {
                const fd = try std.posix.open("/dev/tty", .{ .ACCMODE = .RDONLY }, @as(std.posix.mode_t, 0));
                if (fd != -1) {
                    _ = ioctl(@intCast(fd), std.posix.T.IOCGWINSZ, @intFromPtr(&ws));
                    _ = std.posix.close(@intCast(fd));
                } else {
                    return error.Invalid;
                }
            }

            return ws;
        }

        pub fn getTermios() !Self.termios {
            return try std.posix.tcgetattr(std.posix.STDIN_FILENO);
        }

        pub fn setTermios(t: Self.termios) !void {
            try std.posix.tcsetattr(std.posix.STDIN_FILENO, std.posix.TCSA.NOW, t);
        }

        pub fn clearEchoAndICanon(t: *Self.termios) void {
            t.lflag.ECHO = false;
            t.lflag.ICANON = false;
            t.lflag.ISIG = false;
        }

        pub fn getTermiosCC(t: Self.termios, cc: Self.V) u8 {
            return t.cc[@intFromEnum(cc)];
        }

        pub const POLL_IN = std.posix.POLL.IN;

        pub fn setPollFd(p: *Self.pollfd, f: std.posix.fd_t) void {
            p.fd = f;
        }

        const PollReturnType = if (builtin.link_libc) c_int else usize; // AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        pub const nfds_t = std.posix.nfds_t;
        pub fn poll(fds: [*]Self.pollfd, n: Self.nfds_t, timeout: i32) PollReturnType {
            return std.posix.system.poll(fds, n, timeout);
        }

        pub const pipe = std.posix.pipe;
    },
};

const getTermiosCC = SystemCapabilities.getTermiosCC;
const getTermios = SystemCapabilities.getTermios;
const setTermios = SystemCapabilities.setTermios;
const clearEchoAndICanon = SystemCapabilities.clearEchoAndICanon;
const termios = SystemCapabilities.termios;
const V = SystemCapabilities.V;

fn isAsciiControl(code_point: u32) bool {
    return code_point < 0x20 or code_point == 0x7f;
}

fn isAsciiSpace(code_point: u32) bool {
    return code_point == ' ' or code_point == '\t';
}

fn isAsciiAlpha(code_point: u32) bool {
    return (code_point >= 'a' and code_point <= 'z') or (code_point >= 'A' and code_point <= 'Z');
}

fn isAsciiNumeric(code_point: u32) bool {
    return code_point >= '0' and code_point <= '9';
}

fn isAsciiAlnum(code_point: u32) bool {
    return isAsciiAlpha(code_point) or isAsciiNumeric(code_point);
}

pub fn ctrl(c: u32) u32 {
    return c & 0x3f;
}

fn utf8ValidRange(s: []const u8) usize {
    var len: usize = 0;

    const N = @sizeOf(usize);
    const MASK = 0x80 * (std.math.maxInt(usize) / 0xff);

    var i: usize = 0;
    while (i < s.len) {
        // Fast path for ASCII sequences
        while (i + N <= s.len) : (i += N) {
            const v = std.mem.readInt(usize, s[i..][0..N], builtin.cpu.arch.endian());
            if (v & MASK != 0) break;
            len += N;
        }

        if (i < s.len) {
            const n = std.unicode.utf8ByteSequenceLength(s[i]) catch {
                return i;
            };
            if (i + n > s.len) return i;

            switch (n) {
                1 => {}, // ASCII, no validation needed
                else => _ = std.unicode.utf8Decode(s[i..][0..n]) catch {
                    return i;
                },
            }

            i += n;
            len += 1;
        }
    }

    return i;
}

pub const CompletionSuggestion = struct {};
pub const Style = struct {
    underline: bool = false,
    bold: bool = false,
    italic: bool = false,
    background: Color = Color{ .xterm = XtermColor.Unchanged },
    foreground: Color = Color{ .xterm = XtermColor.Unchanged },
    // FIXME: Masks + Hyperlinks

    const Self = @This();

    pub const XtermColor = enum(i32) {
        Default = 9,
        Black = 0,
        Red,
        Green,
        Yellow,
        Blue,
        Magenta,
        Cyan,
        White,
        Unchanged,
    };

    pub const Color = union(enum) {
        xterm: XtermColor,
        rgb: [3]u8,

        pub fn isDefault(self: @This()) bool {
            return switch (self) {
                .xterm => self.xterm == XtermColor.Unchanged,
                .rgb => self.rgb[0] == 0 and self.rgb[1] == 0 and self.rgb[2] == 0,
            };
        }

        pub fn toVTEscape(self: @This(), allocator: Allocator, role: enum { background, foreground }) ![]const u8 {
            switch (self) {
                .xterm => {
                    if (self.xterm == XtermColor.Unchanged) {
                        return "";
                    }
                    return try std.fmt.allocPrint(
                        allocator,
                        "\x1b[{d}m",
                        .{@intFromEnum(self.xterm) + @as(u8, if (role == .background) 40 else 30)},
                    );
                },
                .rgb => {
                    return try std.fmt.allocPrint(
                        allocator,
                        "\x1b[{};2;{d};{d};{d}m",
                        .{ @as(u8, if (role == .background) 48 else 38), self.rgb[0], self.rgb[1], self.rgb[2] },
                    );
                },
            }
        }
    };

    pub fn resetStyle() Self {
        return .{
            .background = Color{ .xterm = XtermColor.Default },
            .foreground = Color{ .xterm = XtermColor.Default },
        };
    }

    pub fn unifiedWith(self: Self, other: Self, prefer_other: bool) Self {
        var style = self;
        style.unifyWith(other, prefer_other);
        return style;
    }

    pub fn unifyWith(self: *Self, other: Self, prefer_other: bool) void {
        if (prefer_other or self.background.isDefault()) {
            self.background = other.background;
        }

        if (prefer_other or self.foreground.isDefault()) {
            self.foreground = other.foreground;
        }

        if (other.bold) {
            self.bold = true;
        }

        if (other.italic) {
            self.italic = true;
        }

        if (other.underline) {
            self.underline = true;
        }
    }
};
pub const Span = struct {
    const Mode = enum {
        ByteOriented,
        CodePointOriented,
    };
    const Self = @This();

    begin: usize,
    end: usize,
    mode: Mode = .CodePointOriented,

    pub fn isEmpty(self: Self) bool {
        return self.begin >= self.end;
    }
};

pub const StringMetrics = struct {
    pub const LineMetrics = struct {
        length: usize = 0,

        pub fn totalLength(self: @This()) usize {
            return self.length;
        }
    };

    line_metrics: ArrayList(LineMetrics),
    total_length: usize = 0,
    max_line_length: usize = 0,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .line_metrics = ArrayList(LineMetrics).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.line_metrics.deinit();
    }

    pub fn linesWithAddition(self: Self, offset: Self, column_width: usize) usize {
        var lines: usize = 0;

        if (self.line_metrics.size() != 0) {
            for (0..self.line_metrics.size() - 1) |i| {
                lines += (self.line_metrics.container.items[i].totalLength() + column_width) / column_width;
            }

            var last = self.line_metrics.container.items[self.line_metrics.size() - 1].totalLength();
            last += offset.line_metrics.container.items[0].totalLength();
            lines += (last + column_width) / column_width;
        }

        for (1..offset.line_metrics.size()) |i| {
            lines += (offset.line_metrics.container.items[i].totalLength() + column_width) / column_width;
        }

        return lines;
    }

    pub fn offsetWithAddition(self: Self, offset: Self, column_width: usize) usize {
        if (offset.line_metrics.size() > 1) {
            return offset.line_metrics.container.items[offset.line_metrics.size() - 1].totalLength() % column_width;
        }

        if (self.line_metrics.size() != 0) {
            var last = self.line_metrics.container.items[offset.line_metrics.size() - 1].totalLength();
            last += offset.line_metrics.container.items[0].totalLength();
            return last % column_width;
        }

        if (offset.line_metrics.size() == 0) {
            return 0;
        }

        return offset.line_metrics.container.items[0].totalLength() % column_width;
    }

    pub fn reset(self: *Self) !void {
        self.line_metrics.container.clearAndFree();
        self.total_length = 0;
        self.max_line_length = 0;
        try self.line_metrics.container.append(LineMetrics{});
    }
};

pub const SuggestionDisplay = struct {};
pub const SuggestionManager = struct {};
pub const CSIMod = enum(u8) {
    None = 0,
    Shift = 1,
    Alt = 2,
    Ctrl = 4,
};
pub const Key = struct {
    code_point: u32,
    modifiers: enum(u8) {
        None = 0,
        Alt = 1,
    } = .None,

    const Self = @This();

    pub fn equals(self: Self, other: Self) bool {
        return self.code_point == other.code_point and self.modifiers == other.modifiers;
    }
};

const KeyCallbackEntry = struct {
    sequence: ArrayList(Key),
    callback: *const fn (*Editor) bool,

    const Self = @This();

    pub fn init(allocator: Allocator, sequence: []const Key, f: *const fn (*Editor) bool) Self {
        var self = Self{
            .sequence = ArrayList(Key).init(allocator),
            .callback = f,
        };
        self.sequence.container.appendSlice(sequence) catch unreachable;
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.sequence.deinit();
    }
};

pub const KeyCallbackMachine = struct {
    key_callbacks: ArrayList(KeyCallbackEntry),
    current_matching_keys: ArrayList([]const Key),
    sequence_length: usize = 0,
    should_process_this_key: bool = true,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .key_callbacks = ArrayList(KeyCallbackEntry).init(allocator),
            .current_matching_keys = ArrayList([]const Key).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.key_callbacks.deinit();
        self.current_matching_keys.deinit();
    }

    pub fn keyPressed(self: *Self, editor: *Editor, key: Key) !void {
        if (self.sequence_length == 0) {
            std.debug.assert(self.current_matching_keys.size() == 0);

            for (self.key_callbacks.container.items) |entry| {
                if (entry.sequence.container.items[0].equals(key)) {
                    try self.current_matching_keys.container.append(entry.sequence.container.items);
                }
            }

            if (self.current_matching_keys.size() == 0) {
                self.should_process_this_key = true;
                return;
            }
        }

        self.sequence_length += 1;
        var old_matching_keys = ArrayList([]const Key).init(editor.allocator);
        std.mem.swap(@TypeOf(old_matching_keys), &old_matching_keys, &self.current_matching_keys);
        defer old_matching_keys.deinit();

        for (old_matching_keys.container.items) |o_key| {
            if (o_key.len < self.sequence_length) {
                continue;
            }

            if (o_key[self.sequence_length - 1].equals(key)) {
                try self.current_matching_keys.container.append(o_key);
            }
        }

        if (self.current_matching_keys.size() == 0) {
            // Insert any captured keys
            if (old_matching_keys.size() != 0) {
                for (old_matching_keys.container.items[0]) |k| {
                    editor.insertCodePoint(k.code_point);
                }
            }
            self.sequence_length = 0;
            self.should_process_this_key = true;
            return;
        }

        self.should_process_this_key = false;
        for (self.current_matching_keys.container.items) |ks| {
            if (ks.len == self.sequence_length) {
                self.should_process_this_key = self.callback(ks, editor, false);
                self.sequence_length = 0;
                self.current_matching_keys.container.clearAndFree();
                return;
            }
        }
    }

    fn callback(self: *Self, keys: []const Key, editor: *Editor, default: bool) bool {
        for (self.key_callbacks.container.items) |entry| {
            if (entry.sequence.container.items.len == keys.len) {
                var match = true;
                for (entry.sequence.container.items, 0..keys.len) |key, i| {
                    if (!key.equals(keys[i])) {
                        match = false;
                        break;
                    }
                }
                if (match) {
                    return entry.callback(editor);
                }
            }
        }

        return default;
    }

    pub fn registerKeyInputCallback(self: *Self, sequence: []const Key, c: *const fn (*Editor) bool) !void {
        const inserted_entry = KeyCallbackEntry.init(self.key_callbacks.container.allocator, sequence, c);

        for (self.key_callbacks.container.items, 0..self.key_callbacks.size()) |entry, j| {
            if (entry.sequence.container.items.len == sequence.len) {
                var match = true;
                for (entry.sequence.container.items, 0..sequence.len) |key, i| {
                    if (!key.equals(sequence[i])) {
                        match = false;
                        break;
                    }
                }
                if (match) {
                    self.key_callbacks.container.items[j].deinit();
                    self.key_callbacks.container.items[j] = inserted_entry;
                    return;
                }
            }
        }

        try self.key_callbacks.container.append(inserted_entry);
    }

    pub fn shouldProcessLastPressedKey(self: *Self) bool {
        return self.should_process_this_key;
    }
};
pub const HistoryEntry = struct {
    entry: ArrayList(u8),
    timestamp: i64,

    const Self = @This();

    pub fn init(allocator: Allocator, entry: []const u8) !Self {
        var self = Self{
            .entry = ArrayList(u8).init(allocator),
            .timestamp = std.time.timestamp(),
        };
        try self.entry.container.appendSlice(entry);
        return self;
    }

    pub fn initWithTimestamp(allocator: Allocator, entry: []const u8, timestamp: i64) !Self {
        var self = Self{
            .entry = ArrayList(u8).init(allocator),
            .timestamp = timestamp,
        };
        try self.entry.container.appendSlice(entry);
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.entry.deinit();
    }
};
pub const Configuration = struct {
    pub const OperationMode = enum {
        Full,
        NoEscapeSequences,
        NonInteractive,
    };

    enable_bracketed_paste: bool = true,
    operation_mode: OperationMode = SystemCapabilities.default_operation_mode,
    enable_signal_handling: bool = should_enable_signal_handling,
};

fn vtMoveRelative(row: i64, col: i64) !void {
    var x_op: u8 = 'A';
    var y_op: u8 = 'D';
    var r = row;
    var c = col;

    if (row > 0) {
        x_op = 'B';
    } else {
        r = -row;
    }

    if (col > 0) {
        y_op = 'C';
    } else {
        c = -col;
    }

    var writer = SystemCapabilities.getStdErr().writer();
    if (row > 0) {
        try writer.print("\x1b[{d}{c}", .{ r, x_op });
    }
    if (col > 0) {
        try writer.print("\x1b[{d}{c}", .{ c, y_op });
    }
}

var signalHandlingData: ?struct {
    pipe: struct {
        write: std.posix.fd_t,
        read: std.posix.fd_t,
    },
    old_sigint: ?SystemCapabilities.Sigaction = null,
    old_sigwinch: ?SystemCapabilities.Sigaction = null,

    pub fn handleSignal(signo: i32) callconv(.C) void {
        var f = std.fs.File{ .handle = signalHandlingData.?.pipe.write };
        f.writer().writeInt(i32, signo, .little) catch {};
    }
} = null;

pub const Editor = struct {
    pub const Error =
        std.mem.Allocator.Error ||
        std.posix.MMapError ||
        std.posix.OpenError ||
        std.posix.PipeError ||
        std.posix.ReadError ||
        std.posix.RealPathError ||
        std.posix.TermiosGetError ||
        std.posix.TermiosSetError ||
        std.posix.WriteError ||
        error{ CodepointTooLarge, Utf8CannotEncodeSurrogateHalf } ||
        error{ Empty, Eof, ReadFailure } ||
        error{ EndOfStream, StreamTooLong, OperationNotSupported } ||
        error{ SystemResource, ThreadQuotaExceeded };

    pub const Signal = enum {
        SIGWINCH,
    };

    const Self = @This();
    const InputState = enum {
        Free,
        Verbatim,
        Paste,
        GotEscape,
        CSIExpectParameter,
        CSIExpectIntermediate,
        CSIExpectFinal,
    };
    const VTState = enum {
        Free,
        Escape,
        Bracket,
        BracketArgsSemi,
        Title,
    };
    const DrawnSpans = struct {
        starting: AutoHashMap(usize, AutoHashMap(usize, Style)),
        ending: AutoHashMap(usize, AutoHashMap(usize, Style)),

        pub fn init(allocator: Allocator) @This() {
            return .{
                .starting = AutoHashMap(usize, AutoHashMap(usize, Style)).init(allocator),
                .ending = AutoHashMap(usize, AutoHashMap(usize, Style)).init(allocator),
            };
        }

        pub fn deinit(self: *@This()) void {
            self.starting.deinit();
            self.ending.deinit();
        }

        pub fn containsUpToOffset(self: @This(), other: @This(), offset: usize) bool {
            _ = self;
            _ = other;
            _ = offset;
            return false;
        }

        pub fn deepCopy(self: *const @This()) !@This() {
            var other = @This().init(self.starting.container.allocator);
            var start_it = self.starting.container.iterator();
            while (start_it.next()) |start| {
                var inner_it = start.value_ptr.container.iterator();
                var hm = AutoHashMap(usize, Style).init(self.starting.container.allocator);
                try other.starting.container.put(start.key_ptr.*, hm);
                while (inner_it.next()) |inner| {
                    try hm.container.put(inner.key_ptr.*, inner.value_ptr.*);
                }
            }

            var end_it = self.ending.container.iterator();
            while (end_it.next()) |end| {
                var inner_it = end.value_ptr.container.iterator();
                var hm = AutoHashMap(usize, Style).init(self.ending.container.allocator);
                try other.ending.container.put(end.key_ptr.*, hm);
                while (inner_it.next()) |inner| {
                    try hm.container.put(inner.key_ptr.*, inner.value_ptr.*);
                }
            }

            return other;
        }
    };
    const LoopExitCode = enum {
        Exit,
        Retry,
    };
    const DeferredAction = union(enum) {
        HandleResizeEvent: bool, // reset_origin
        TryUpdateOnce: u8, // dummy
    };
    const Callback = struct {
        f: *const fn (*anyopaque) void,
        context: *anyopaque,

        pub fn makeHandler(comptime T: type, comptime InnerT: type, comptime name: []const u8) type {
            return struct {
                pub fn theHandler(context: *anyopaque) void {
                    const ctx: T = @alignCast(@ptrCast(context));
                    @call(.auto, @field(InnerT, name), .{ctx});
                }
            };
        }
    };
    fn Callback1(comptime T: type) type {
        return struct {
            f: *const fn (*anyopaque, T) void,
            context: *anyopaque,

            pub fn makeHandler(comptime U: type, comptime InnerT: type, comptime name: []const u8) type {
                return struct {
                    pub fn theHandler(context: *anyopaque, value: T) void {
                        const ctx: U = @alignCast(@ptrCast(context));
                        @call(.auto, @field(InnerT, name), .{ ctx, value });
                    }
                };
            }
        };
    }

    on: struct {
        display_refresh: ?Callback = null,
        paste: ?Callback1([]const u32) = null,
    } = .{},

    allocator: Allocator,
    buffer: ArrayList(u32),
    finished: bool = false,
    search_editor: ?*Self = null,
    is_searching: bool = false,
    reset_buffer_on_search_end: bool = false,
    search_offset: usize = 0,
    search_offset_state: enum {
        Unbiased,
        Backwards,
        Forwards,
    } = .Unbiased,
    pre_search_cursor: usize = 0,
    pre_search_buffer: ArrayList(u32),
    pending_chars: ArrayList(u8),
    incomplete_data: ArrayList(u8),
    input_error: ?Wrapped(Error) = null, // ?Error behaves weirdly - `null` seems to be equal to whatever error number 0 represents, so the null state cannot be represented at all.
    returned_line: []const u8,
    cursor: usize = 0,
    drawn_cursor: usize = 0,
    drawn_end_of_line_offset: usize = 0,
    inline_search_cursor: usize = 0,
    chars_touched_in_the_middle: usize = 0,
    times_tab_pressed: usize = 0,
    num_columns: usize = 0,
    num_lines: usize = 0,
    previous_num_columns: usize = 0,
    extra_forward_lines: usize = 0,
    shown_lines: usize = 0,
    cached_prompt_metrics: StringMetrics,
    old_prompt_metrics: StringMetrics,
    cached_buffer_metrics: StringMetrics,
    prompt_lines_at_suggestion_initiation: usize = 0,
    cached_prompt_valid: bool = false,
    origin_row: usize = 0,
    origin_column: usize = 0,
    has_origin_reset_scheduled: bool = false,
    suggestion_display: ?SuggestionDisplay = null,
    remembered_suggestion_static_data: ArrayList(u32),
    new_prompt: ArrayList(u8),
    suggestion_manager: SuggestionManager = .{},
    always_refresh: bool = false,
    tab_direction: enum {
        Forward,
        Backward,
    } = .Forward,
    callback_machine: KeyCallbackMachine,
    termios: termios = undefined,
    default_termios: termios = undefined,
    was_interrupted: bool = false,
    was_resized: bool = false,
    history: ArrayList(HistoryEntry),
    history_cursor: usize = 0,
    history_capacity: usize = 1024,
    history_dirty: bool = false,
    input_state: InputState = .Free,
    previous_free_state: InputState = .Free,
    current_spans: DrawnSpans,
    paste_buffer: ArrayList(u32),
    initialized: bool = false,
    refresh_needed: bool = false,
    signal_handlers: [2]i32 = .{ 0, 0 },
    is_editing: bool = false,
    prohibit_input_processing: bool = false,
    have_unprocessed_read_event: bool = false,
    configuration: Configuration,
    event_loop: Loop,

    const Loop = if (builtin.os.tag == .wasi or builtin.os.tag == .uefi)
        struct {
            pub fn init(allocator: Allocator, configuration: Configuration) Loop {
                _ = configuration;
                _ = allocator;
                return .{};
            }

            pub fn restart(self: *Loop) !void {
                _ = self;
            }

            pub fn stop(self: *Loop) !void {
                _ = self;
            }

            pub fn pump(self: *Loop) void {
                _ = self;
            }

            pub const Process = struct {
                pub fn done(self: *@This()) void {
                    _ = self;
                }
            };

            pub fn process(self: *Loop) !Process {
                _ = self;
                return Process{};
            }

            pub fn loopAction(self: *Loop, code: LoopExitCode) !void {
                _ = self;
                _ = code;
            }

            pub fn deferredInvoke(self: *Loop, action: DeferredAction) !void {
                _ = self;
                _ = action;
            }

            pub fn deinit(self: *Loop) void {
                _ = self;
            }

            pub const HandleResult = struct {
                e: ?Wrapped(error{ ZiglineEventLoopExit, ZiglineEventLoopRetry }),
            };

            pub fn handle(self: *Loop, handlers: anytype) !HandleResult {
                _ = self;
                _ = handlers;
                return HandleResult{ .e = null };
            }
        }
    else
        struct {
            enable_signal_handling: bool,
            control_thread: ?Thread = null,
            control_thread_exited: bool = false,
            thread_kill_pipe: ?struct {
                write: std.posix.fd_t,
                read: std.posix.fd_t,
            } = null,
            queue_cond_mutex: Mutex = .{},
            queue_condition: Condition = .{},
            logic_cond_mutex: Mutex = .{},
            logic_condition: Condition = .{},
            loop_queue: Queue(LoopExitCode),
            deferred_action_queue: Queue(DeferredAction),
            signal_queue: Queue(Signal),
            input_error: ?Wrapped(Error) = null,

            pub fn init(allocator: Allocator, configuration: Configuration) Loop {
                return .{
                    .enable_signal_handling = configuration.enable_signal_handling,
                    .loop_queue = Queue(LoopExitCode).init(allocator),
                    .deferred_action_queue = Queue(DeferredAction).init(allocator),
                    .signal_queue = Queue(Signal).init(allocator),
                };
            }

            pub fn restart(self: *Loop) !void {
                try self.stop();
                self.control_thread_exited = false;
                self.control_thread = try Thread.spawn(.{}, Loop.controlThreadMain, .{self});
            }

            pub fn stop(self: *Loop) !void {
                if (self.control_thread) |t| {
                    self.nicelyAskControlThreadToDie() catch {};
                    self.logic_condition.broadcast();
                    t.join();
                }
            }

            pub fn pump(self: *Loop) void {
                self.queue_condition.wait(&self.queue_cond_mutex);
            }

            fn nicelyAskControlThreadToDie(self: *Loop) !void {
                if (self.control_thread_exited) {
                    return;
                }

                // In the absence of way to interrupt threads, we're just gonna write to it and hope it dies on its own pace.
                if (self.thread_kill_pipe) |pipes| {
                    _ = std.posix.write(pipes.write, "x") catch 0;
                }
            }

            pub fn deinit(self: *Loop) void {
                self.loop_queue.deinit();
                self.deferred_action_queue.deinit();
                self.signal_queue.deinit();
            }

            const Process = struct {
                loop: *Loop,
                pub fn done(self: *@This()) void {
                    self.loop.queue_cond_mutex.unlock();
                }
            };

            pub fn process(self: *Loop) !Process {
                self.queue_cond_mutex.lock();

                if (self.thread_kill_pipe == null) {
                    const pipe = try SystemCapabilities.pipe();
                    self.thread_kill_pipe = .{
                        .read = pipe[0],
                        .write = pipe[1],
                    };
                }

                return Process{ .loop = self };
            }

            pub fn loopAction(self: *Loop, code: LoopExitCode) !void {
                try self.loop_queue.enqueue(code);
                self.queue_condition.broadcast();
            }

            pub fn deferredInvoke(self: *Loop, action: DeferredAction) !void {
                try self.deferred_action_queue.enqueue(action);
            }

            fn controlThreadMain(self: *Loop) void {
                defer self.control_thread_exited = true;

                const stdin = SystemCapabilities.getStdIn();

                std.debug.assert(self.thread_kill_pipe != null);

                var pollfds = [_]SystemCapabilities.pollfd{ undefined, undefined, undefined };
                SystemCapabilities.setPollFd(&pollfds[0], stdin.handle);
                SystemCapabilities.setPollFd(&pollfds[1], self.thread_kill_pipe.?.read);
                pollfds[0].events = SystemCapabilities.POLL_IN;
                pollfds[1].events = SystemCapabilities.POLL_IN;
                pollfds[2].events = 0;

                var nfds: SystemCapabilities.nfds_t = 2;

                if (self.enable_signal_handling) {
                    SystemCapabilities.setPollFd(&pollfds[2], signalHandlingData.?.pipe.read);
                    pollfds[2].events = SystemCapabilities.POLL_IN;
                    nfds = 3;
                }

                defer self.queue_condition.broadcast();

                while (true) {
                    self.logic_cond_mutex.lock();

                    {
                        defer self.logic_cond_mutex.unlock();
                        const rc = SystemCapabilities.poll(&pollfds, nfds, std.math.maxInt(i32));
                        if (rc < 0) {
                            self.input_error = switch (std.posix.errno(rc)) {
                                .INTR => {
                                    continue;
                                },
                                .NOMEM => toWrapped(Error, error.SystemResource),
                                else => {
                                    unreachable;
                                },
                            };
                            self.loop_queue.enqueue(.Exit) catch {
                                break;
                            };
                            self.queue_condition.broadcast();
                            break;
                        }
                    }

                    if (pollfds[1].revents & SystemCapabilities.POLL_IN != 0) {
                        // We're supposed to die...after draining the pipe.
                        var buf = [_]u8{0} ** 8;
                        _ = std.posix.read(self.thread_kill_pipe.?.read, &buf) catch 0;
                        break;
                    }

                    if (!is_windows) {
                        if (pollfds[2].revents & SystemCapabilities.POLL_IN != 0) no_read: {
                            // A signal! Let's handle it.
                            var f = std.fs.File{ .handle = signalHandlingData.?.pipe.read };
                            const signo = f.reader().readInt(i32, .little) catch {
                                break :no_read;
                            };
                            switch (signo) {
                                std.posix.SIG.WINCH => {
                                    self.signal_queue.enqueue(.SIGWINCH) catch {
                                        break :no_read;
                                    };
                                },
                                else => {
                                    break :no_read;
                                },
                            }
                            self.queue_condition.broadcast();
                            // Wait for the main thread to process the event, any further input between now and then will be
                            // picked up either immediately, or in the next cycle.
                            self.logic_cond_mutex.lock();
                            defer self.logic_cond_mutex.unlock();
                            self.logic_condition.wait(&self.logic_cond_mutex);
                        }
                    }

                    if (pollfds[0].revents & SystemCapabilities.POLL_IN != 0) {
                        self.deferred_action_queue.enqueue(DeferredAction{ .TryUpdateOnce = 0 }) catch {};
                        self.logic_cond_mutex.lock();
                        defer self.logic_cond_mutex.unlock();
                        self.queue_condition.broadcast();
                        // Wait for the main thread to process the event, any further input between now and then will be
                        // picked up either immediately, or in the next cycle.
                        self.logic_condition.wait(&self.logic_cond_mutex);
                    }
                }
            }

            pub const HandleResult = struct {
                const Error = error{ ZiglineEventLoopExit, ZiglineEventLoopRetry };
                e: ?Wrapped(@This().Error),
            };
            fn handleSignalOrLoopAction(self: *Loop, handlers: anytype) !void {
                while (!self.signal_queue.isEmpty()) {
                    const signal = self.signal_queue.dequeue();
                    try handlers.signal.handleEvent(signal);
                }

                while (!self.loop_queue.isEmpty()) {
                    const code = self.loop_queue.dequeue();
                    switch (code) {
                        .Exit => {
                            return error.ZiglineEventLoopExit;
                        },
                        .Retry => {
                            return error.ZiglineEventLoopRetry;
                        },
                    }
                }
            }

            pub fn handle(self: *Loop, handlers: anytype) !HandleResult {
                defer self.logic_condition.broadcast();

                while (!self.signal_queue.isEmpty() or !self.loop_queue.isEmpty() or !self.deferred_action_queue.isEmpty()) {
                    try self.handleSignalOrLoopAction(handlers);

                    while (!self.deferred_action_queue.isEmpty()) {
                        const action = self.deferred_action_queue.dequeue();
                        try handlers.deferred_action.handleEvent(action);
                        try self.handleSignalOrLoopAction(handlers);
                    }

                    if (self.input_error) |e| {
                        return fromWrapped(e);
                    }
                }

                return .{ .e = null };
            }
        };

    pub fn init(allocator: Allocator, configuration: Configuration) Self {
        const self = Self{
            .allocator = allocator,
            .buffer = ArrayList(u32).init(allocator),
            .callback_machine = KeyCallbackMachine.init(allocator),
            .pre_search_buffer = ArrayList(u32).init(allocator),
            .pending_chars = ArrayList(u8).init(allocator),
            .incomplete_data = ArrayList(u8).init(allocator),
            .returned_line = &[0]u8{},
            .remembered_suggestion_static_data = ArrayList(u32).init(allocator),
            .history = ArrayList(HistoryEntry).init(allocator),
            .current_spans = DrawnSpans.init(allocator),
            .new_prompt = ArrayList(u8).init(allocator),
            .paste_buffer = ArrayList(u32).init(allocator),
            .configuration = configuration,
            .cached_prompt_metrics = StringMetrics.init(allocator),
            .old_prompt_metrics = StringMetrics.init(allocator),
            .cached_buffer_metrics = StringMetrics.init(allocator),
            .event_loop = Loop.init(allocator, configuration),
        };

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.event_loop.stop() catch {};
        self.buffer.deinit();
        self.pre_search_buffer.deinit();
        self.pending_chars.deinit();
        self.incomplete_data.deinit();
        self.remembered_suggestion_static_data.deinit();
        self.history.deinit();
        self.new_prompt.deinit();
        self.paste_buffer.deinit();
        self.current_spans.deinit();
        self.callback_machine.deinit();
        self.cached_buffer_metrics.deinit();
        self.cached_prompt_metrics.deinit();
        self.event_loop.deinit();
    }

    pub fn reFetchDefaultTermios(self: *Self) !void {
        const t = try getTermios();
        self.default_termios = t;
        if (self.configuration.operation_mode == .Full) {
            clearEchoAndICanon(&t);
        }
        self.termios = t;
    }

    pub fn addToHistory(self: *Self, line: []const u8) !void {
        const entry = try HistoryEntry.init(self.allocator, line);
        try self.history.container.append(entry);
    }

    pub fn loadHistory(self: *Self, path: []const u8) !void {
        var history_file = std.fs.cwd().openFile(path, .{}) catch |err| switch (err) {
            error.FileNotFound => {
                return;
            },
            else => {
                return err;
            },
        };
        defer history_file.close();
        const data = try history_file.reader().readAllAlloc(self.allocator, std.math.maxInt(usize));
        defer self.allocator.free(data);
        var it = std.mem.splitSequence(u8, data, "\n\n");
        while (it.next()) |str| {
            if (str.len == 0 or (str.len == 1 and str[0] == '\n')) {
                continue;
            }

            const entry_start = std.mem.indexOf(u8, str, "::") orelse 0;
            const timestamp = std.fmt.parseInt(i64, str[0..entry_start], 10) catch 0;
            const entry = try HistoryEntry.initWithTimestamp(
                self.allocator,
                str[(if (entry_start == 0) 0 else entry_start + 2)..],
                timestamp,
            );
            try self.history.container.append(entry);
        }
    }

    pub fn saveHistory(self: *Self, path: []const u8) !void {
        var history_file = try std.fs.cwd().createFile(path, .{});
        defer history_file.close();
        var writer = history_file.writer();
        for (self.history.container.items) |entry| {
            try writer.print("{}::{s}\n\n", .{ entry.timestamp, entry.entry.container.items });
        }
    }

    pub fn stylize(self: *Self, span: Span, style: Style) !void {
        if (span.isEmpty()) {
            return;
        }

        var start = span.begin;
        var end = span.end;

        if (span.mode == .ByteOriented) {
            const offsets = self.byteOffsetRangeToCodePointOffsetRange(span.begin, span.end, 0, false);
            start = offsets.start;
            end = offsets.end;
        }

        var spans_starting = &self.current_spans.starting.container;
        var spans_ending = &self.current_spans.ending.container;

        var put_result = try spans_starting.getOrPut(start);
        var starting_map = put_result.value_ptr;
        if (!put_result.found_existing) {
            starting_map.* = AutoHashMap(usize, Style).init(self.allocator);
        }

        var maxKnownEnd: usize = 0;
        var startIt = starting_map.container.keyIterator();
        while (startIt.next()) |kp| {
            const key = kp.*;
            if (key > maxKnownEnd) {
                maxKnownEnd = key;
            }
        }
        if (!starting_map.container.contains(end) and maxKnownEnd > end) {
            self.refresh_needed = true;
        }
        try starting_map.container.put(end, style);

        put_result = try spans_ending.getOrPut(end);
        var ending_map = put_result.value_ptr;
        if (!put_result.found_existing) {
            ending_map.* = AutoHashMap(usize, Style).init(self.allocator);
        }

        var minKnownStart: usize = std.math.maxInt(usize);
        var maxKnownStart: usize = 0;
        var endIt = ending_map.container.keyIterator();
        while (endIt.next()) |kp| {
            const key = kp.*;
            if (key < minKnownStart) {
                minKnownStart = key;
            }
            if (key > maxKnownStart) {
                maxKnownStart = key;
            }
        }
        if (!ending_map.container.contains(start) and (minKnownStart < start or maxKnownStart > start)) {
            self.refresh_needed = true;
        }
        try ending_map.container.put(start, style);
    }

    pub fn stripStyles(self: *Self) void {
        self.current_spans.deinit();
        self.current_spans = DrawnSpans.init(self.allocator);
    }

    fn getLineNonInteractive(self: *Self, prompt: []const u8) ![]const u8 {
        _ = try SystemCapabilities.getStdErr().write(prompt);

        var array = ArrayList(u8).init(self.allocator);
        defer array.deinit();

        streamUntilEol(SystemCapabilities.getStdIn().reader(), array.container.writer()) catch |e| switch (e) {
            error.EndOfStream => return error.Eof,
            else => return e,
        };

        return array.container.toOwnedSlice();
    }

    fn streamUntilEol(reader: anytype, writer: anytype) !void {
        if (!is_windows) {
            // eol is '\n', so we can just use streamUntilDelimiter.
            return reader.streamUntilDelimiter(writer, '\n', null);
        }

        // Read until '\r', return if '\n' follows, otherwise keep reading.
        while (true) {
            try reader.streamUntilDelimiter(writer, '\r', null);
            const byte = try reader.readByte();
            if (byte == '\n')
                return;
            try writer.writeByte('\r');
            try writer.writeByte(byte);
        }
    }

    pub fn getLine(self: *Self, prompt: []const u8) ![]const u8 {
        if (self.configuration.operation_mode == .NonInteractive) {
            // Disable all the fancy stuff, use a plain read.
            return self.getLineNonInteractive(prompt);
        }

        var loop_handle = try self.event_loop.process();
        defer loop_handle.done();

        if (should_enable_signal_handling) {
            if (self.configuration.enable_signal_handling) {
                if (signalHandlingData == null) {
                    const pipe = try SystemCapabilities.pipe();
                    signalHandlingData = .{
                        .pipe = .{
                            .read = pipe[0],
                            .write = pipe[1],
                        },
                    };

                    signalHandlingData.?.old_sigint = @as(SystemCapabilities.Sigaction, undefined);
                    signalHandlingData.?.old_sigwinch = @as(SystemCapabilities.Sigaction, undefined);
                    std.posix.sigaction(
                        std.posix.SIG.INT,
                        &SystemCapabilities.Sigaction{ .handler = .{ .handler = @TypeOf(signalHandlingData.?).handleSignal }, .mask = std.posix.empty_sigset, .flags = 0 },
                        &signalHandlingData.?.old_sigint.?,
                    );
                    std.posix.sigaction(
                        std.posix.SIG.WINCH,
                        &SystemCapabilities.Sigaction{ .handler = .{ .handler = @TypeOf(signalHandlingData.?).handleSignal }, .mask = std.posix.empty_sigset, .flags = 0 },
                        &signalHandlingData.?.old_sigwinch.?,
                    );
                }
            }
        }

        start: while (true) {
            try self.initialize();

            self.is_editing = true;
            const old_cols = self.num_columns;
            const old_lines = self.num_lines;
            self.getTerminalSize();

            var stderr = SystemCapabilities.getStdErr();

            if (self.configuration.enable_bracketed_paste) {
                try stderr.writeAll("\x1b[?2004h");
            }

            if (self.num_columns != old_cols or self.num_lines != old_lines) {
                self.refresh_needed = true;
            }

            try self.setPrompt(prompt);
            try self.reset();
            self.stripStyles();

            const prompt_lines = @max(self.currentPromptMetrics().line_metrics.container.items.len, 1) - 1;
            for (0..prompt_lines) |_| {
                try stderr.writeAll("\n");
            }

            try vtMoveRelative(-@as(i64, @intCast(prompt_lines)), 0);
            _ = self.setOrigin(true);

            self.history_cursor = self.history.container.items.len;

            try self.refreshDisplay();

            try self.event_loop.restart();

            var had_incomplete_data_at_start = false;
            if (self.incomplete_data.container.items.len != 0) {
                try self.event_loop.deferredInvoke(.{ .TryUpdateOnce = 0 });
                had_incomplete_data_at_start = true;
            }

            // FIXME: Install signal handlers.

            while (true) {
                if (!had_incomplete_data_at_start) {
                    self.event_loop.pump();
                }
                had_incomplete_data_at_start = false;

                const result = self.event_loop.handle(.{
                    .signal = struct {
                        editor: *Self,

                        pub fn handleEvent(s: @This(), event: Signal) !void {
                            switch (event) {
                                .SIGWINCH => {
                                    try s.editor.resized();
                                },
                            }
                        }
                    }{ .editor = self },

                    .deferred_action = struct {
                        editor: *Self,
                        pub fn handleEvent(s: @This(), action: DeferredAction) !void {
                            switch (action) {
                                .HandleResizeEvent => {
                                    try s.editor.handleResizeEvent(action.HandleResizeEvent);
                                },
                                .TryUpdateOnce => {
                                    try s.editor.tryUpdateOnce();
                                },
                            }
                        }
                    }{ .editor = self },
                }) catch |e| switch (e) {
                    error.ZiglineEventLoopExit, error.ZiglineEventLoopRetry => Self.Loop.HandleResult{ .e = toWrapped(Self.Loop.HandleResult.Error, e) },
                    else => b: {
                        self.input_error = toWrapped(Error, e);
                        break :b Self.Loop.HandleResult{ .e = null };
                    },
                };

                if (result.e) |err| {
                    switch (fromWrapped(err)) {
                        error.ZiglineEventLoopExit => {
                            self.finished = false;
                            if (self.input_error) |e| {
                                return fromWrapped(e);
                            }
                            return self.returned_line;
                        },
                        error.ZiglineEventLoopRetry => {
                            continue :start;
                        },
                    }
                }
            }
        }
    }

    fn initialize(self: *Self) !void {
        if (self.initialized) {
            return;
        }

        var t = try getTermios();
        self.default_termios = t;

        self.getTerminalSize();

        if (self.configuration.operation_mode == .Full) {
            clearEchoAndICanon(&t);
        }

        try setTermios(t);
        self.termios = t;
        try self.setDefaultKeybinds();
        self.initialized = true;
    }

    pub fn interrupted(self: *Self) Error!bool {
        if (self.is_searching) {
            return self.search_editor.?.interrupted();
        }

        if (!self.is_editing) {
            return false;
        }

        _ = SystemCapabilities.getStdErr().write("^C") catch 0;

        self.buffer.container.clearAndFree();
        self.chars_touched_in_the_middle = 0;
        self.cursor = 0;

        {
            var stream = std.io.bufferedWriter(SystemCapabilities.getStdErr().writer());
            try self.repositionCursor(&stream, true);
            // FIXME: Suggestion display cleanup.
            _ = try stream.write("\n");
            try stream.flush();
        }

        self.was_interrupted = true;

        self.buffer.container.clearAndFree();
        self.chars_touched_in_the_middle = 0;
        self.is_editing = false;
        try self.restore();
        try self.event_loop.loopAction(.Retry);

        return false;
    }

    pub fn resized(self: *Self) Error!void {
        self.was_resized = true;
        self.previous_num_columns = self.num_columns;
        self.getTerminalSize();

        if (!self.has_origin_reset_scheduled) {
            // Reset the origin, but make sure it doesn't blow up if we fail to read it.
            if (self.setOrigin(false)) {
                try self.handleResizeEvent(false);
            } else {
                try self.event_loop.deferredInvoke(.{ .HandleResizeEvent = true });
                self.has_origin_reset_scheduled = true;
            }
        }
    }

    pub fn getCursor(self: *Self) usize {
        return self.cursor;
    }

    pub fn setCursor(self: *Self, cursor: usize) void {
        if (cursor > self.buffer.container.items.len) {
            self.cursor = self.buffer.container.items.len;
        } else {
            self.cursor = cursor;
        }
    }

    pub fn getBuffer(self: *Self) []const u32 {
        return self.buffer.container.items;
    }

    pub fn getBufferedLine(self: *Self) ![]const u8 {
        return self.getBufferedLineUpTo(self.getBuffer().len);
    }

    pub fn getBufferedLineUpTo(self: *Self, index: usize) ![]const u8 {
        var u8buffer = std.ArrayList(u8).init(self.allocator);
        defer u8buffer.deinit();

        for (self.buffer.container.items[0..index]) |code_point| {
            var u8buf = [4]u8{ 0, 0, 0, 0 };
            const length = try std.unicode.utf8Encode(@intCast(code_point), &u8buf);
            try u8buffer.appendSlice(u8buf[0..length]);
        }

        return u8buffer.toOwnedSlice();
    }

    pub fn setPrompt(self: *Self, prompt: []const u8) !void {
        if (self.cached_prompt_valid) {
            self.old_prompt_metrics = self.cached_prompt_metrics;
        }
        self.cached_prompt_valid = false;
        self.cached_prompt_metrics.deinit();
        self.cached_prompt_metrics = try self.actualRenderedStringMetrics(prompt);
        self.new_prompt.container.clearRetainingCapacity();
        try self.new_prompt.container.appendSlice(prompt);
    }

    fn actualRenderedStringLengthStep(
        metrics: *StringMetrics,
        current_line: *StringMetrics.LineMetrics,
        c: u32,
        next_c: u32,
        state: VTState,
    ) !VTState {
        switch (state) {
            .Free => {
                if (c == 0x1b) {
                    return .Escape;
                }
                if (c == '\r') {
                    current_line.length = 0;
                    if (metrics.line_metrics.size() != 0) {
                        metrics.line_metrics.container.items[metrics.line_metrics.size() - 1] = .{};
                    }
                    return state;
                }
                if (c == '\n') {
                    try metrics.line_metrics.container.append(current_line.*);
                    current_line.length = 0;
                    return state;
                }
                current_line.length += 1;
                metrics.total_length += 1;
                return state;
            },
            .Escape => {
                if (c == ']') {
                    if (next_c == '0') {
                        return .Title;
                    }
                    return state;
                }
                if (c == '[') {
                    return .Bracket;
                }
                return state;
            },
            .Bracket => {
                if (c >= '0' and c <= '9') {
                    return .BracketArgsSemi;
                }
                return state;
            },
            .BracketArgsSemi => {
                if (c == ';') {
                    return .Bracket;
                }
                if (c >= '0' and c <= '9') {
                    return state;
                }
                return .Free;
            },
            .Title => {
                if (c == 7) {
                    return .Free;
                }
                return state;
            },
        }
    }

    pub fn actualRenderedStringMetrics(self: *Self, string: []const u8) !StringMetrics {
        var metrics = StringMetrics.init(self.allocator);
        var current_line = StringMetrics.LineMetrics{};

        var state: VTState = .Free;

        for (0..string.len) |i| {
            const c = string[i];
            var next_c: u32 = 0;
            if (i + 1 < string.len) {
                next_c = string[i + 1];
            }
            state = try actualRenderedStringLengthStep(&metrics, &current_line, c, next_c, state);
        }

        try metrics.line_metrics.container.append(current_line);
        for (metrics.line_metrics.container.items) |line_metric| {
            metrics.max_line_length = @max(line_metric.totalLength(), metrics.max_line_length);
        }

        return metrics;
    }

    pub fn actualRenderedUnicodeStringMetrics(self: *Self, string: []const u32) !StringMetrics {
        var metrics = StringMetrics.init(self.allocator);
        var current_line = StringMetrics.LineMetrics{};

        var state: VTState = .Free;

        for (0..string.len) |i| {
            const c = string[i];
            var next_c: u32 = 0;
            if (i + 1 < string.len) {
                next_c = string[i + 1];
            }
            state = try actualRenderedStringLengthStep(&metrics, &current_line, c, next_c, state);
        }

        try metrics.line_metrics.container.append(current_line);
        for (metrics.line_metrics.container.items) |line_metric| {
            metrics.max_line_length = @max(line_metric.totalLength(), metrics.max_line_length);
        }

        return metrics;
    }

    pub fn clearLine(self: *Self) void {
        _ = self;
        _ = SystemCapabilities.getStdErr().write("\r\x1b[K") catch unreachable;
    }

    pub fn insertString(self: *Self, string: []const u8) void {
        for (string) |code_point| {
            self.insertCodePoint(code_point);
        }
    }

    pub fn insertCodePoint(self: *Self, code_point: u32) void {
        var buf = [_]u8{ 0, 0, 0, 0 };
        const length = std.unicode.utf8Encode(@intCast(code_point), &buf) catch {
            return;
        };
        self.pending_chars.container.appendSlice(buf[0..length]) catch {
            return;
        };

        if (self.cursor == self.buffer.size()) {
            self.buffer.container.append(code_point) catch unreachable;
            self.cursor = self.buffer.size();
            self.inline_search_cursor = self.cursor;
            return;
        }

        self.buffer.container.insert(self.cursor, code_point) catch unreachable;
        self.chars_touched_in_the_middle += 1;
        self.cursor += 1;
        self.inline_search_cursor = self.cursor;
    }

    pub fn insertUtf32(self: *Self, utf32: []const u32) void {
        for (utf32) |code_point| {
            self.insertCodePoint(code_point);
        }
    }

    pub fn finish(self: *Self) bool {
        self.finished = true;
        return false;
    }

    pub fn isEditing(self: *Self) void {
        return self.is_editing;
    }

    pub fn prohibitInput(self: *Self) void {
        self.prohibit_input_processing = true;
    }

    pub fn allowInput(self: *Self) void {
        self.prohibit_input_processing = false;
    }

    fn eatErrors(comptime f: fn (*Self) Error!bool) fn (*Self) bool {
        return struct {
            pub fn handler(self: *Self) bool {
                return f(self) catch false;
            }
        }.handler;
    }

    fn setDefaultKeybinds(self: *Self) !void {
        try self.registerCharInputCallback('\n', &Self.finish);
        try self.registerCharInputCallback(ctrl('C'), &eatErrors(Self.interrupted));

        // ^N searchForwards, ^P searchBackwards
        try self.registerCharInputCallback(ctrl('N'), &Self.searchForwards);
        try self.registerCharInputCallback(ctrl('P'), &Self.searchBackwards);
        // ^A goHome, ^B cursorLeftCharacter
        try self.registerCharInputCallback(ctrl('A'), &Self.goHome);
        try self.registerCharInputCallback(ctrl('B'), &Self.cursorLeftCharacter);
        // ^D eraseCharacterForwards, ^E goEnd
        try self.registerCharInputCallback(ctrl('D'), &Self.eraseCharacterForwards);
        try self.registerCharInputCallback(ctrl('E'), &Self.goEnd);
        // ^F cursorRightCharacter, ^H eraseCharacterBackwards
        try self.registerCharInputCallback(ctrl('F'), &Self.cursorRightCharacter);
        try self.registerCharInputCallback(ctrl('H'), &Self.eraseCharacterBackwards);
        // DEL, some terminals send this instead of ctrl('H')
        try self.registerCharInputCallback(127, &Self.eraseCharacterBackwards);
        // ^K eraseToEnd, ^L clearScreen, ^R enterSearch
        try self.registerCharInputCallback(ctrl('K'), &Self.eraseToEnd);
        try self.registerCharInputCallback(ctrl('L'), &Self.clearScreen);
        // FIXME: try self.registerCharInputCallback(ctrl('R'), &Self.enterSearch);
        // ^T transposeCharacters
        try self.registerCharInputCallback(ctrl('T'), &Self.transposeCharacters);

        // ^[b cursorLeftWord, ^[f cursorRightWord
        try self.registerKeyInputCallback(Key{ .code_point = 'b', .modifiers = .Alt }, &Self.cursorLeftWord);
        try self.registerKeyInputCallback(Key{ .code_point = 'f', .modifiers = .Alt }, &Self.cursorRightWord);
        // ^[^B cursorLeftNonspaceWord, ^[^F cursorRightNonspaceWord
        try self.registerKeyInputCallback(Key{ .code_point = ctrl('B'), .modifiers = .Alt }, &Self.cursorLeftNonspaceWord);
        try self.registerKeyInputCallback(Key{ .code_point = ctrl('F'), .modifiers = .Alt }, &Self.cursorRightNonspaceWord);
        // ^[^H eraseAlnumWordBackwards
        try self.registerKeyInputCallback(Key{ .code_point = ctrl('H'), .modifiers = .Alt }, &Self.eraseAlnumWordBackwards);
        // ^[d eraseAlnumWordForwards
        try self.registerKeyInputCallback(Key{ .code_point = 'd', .modifiers = .Alt }, &Self.eraseAlnumWordForwards);
        // ^[c capitalizeWord, ^[l lowercaseWord, ^[u uppercaseWord, ^[t transposeWords
        try self.registerKeyInputCallback(Key{ .code_point = 'c', .modifiers = .Alt }, &Self.capitalizeWord);
        try self.registerKeyInputCallback(Key{ .code_point = 'l', .modifiers = .Alt }, &Self.lowercaseWord);
        try self.registerKeyInputCallback(Key{ .code_point = 'u', .modifiers = .Alt }, &Self.uppercaseWord);
        // FIXME: try self.registerKeyInputCallback(Key{ .code_point = 't', .modifiers = .Alt }, &Self.transposeWords);

        // Normally ^W eraseWordBackwards
        // Normally ^U killLine
        try self.registerCharInputCallback(ctrl('W'), &Self.eraseWordBackwards);
        try self.registerCharInputCallback(getTermiosCC(self.termios, V.KILL), &Self.killLine);
        try self.registerCharInputCallback(getTermiosCC(self.termios, V.ERASE), &Self.eraseCharacterBackwards);
    }

    fn registerKeyInputCallback(self: *Self, key: Key, c: *const fn (*Editor) bool) !void {
        try self.callback_machine.registerKeyInputCallback(&[1]Key{key}, c);
    }

    fn registerKeySequenceInputCallback(self: *Self, seq: []const Key, c: *const fn (*Editor) bool) !void {
        try self.callback_machine.registerKeyInputCallback(seq, c);
    }

    fn registerCharInputCallback(self: *Self, c: u32, f: *const fn (*Editor) bool) !void {
        try self.callback_machine.registerKeyInputCallback(&[1]Key{Key{ .code_point = c }}, f);
    }

    fn vtApplyStyle(self: *Self, style: Style, output_stream: anytype, is_starting: bool) !void {
        _ = self;

        var buffer: [128]u8 = undefined;
        var a = std.heap.FixedBufferAllocator.init(&buffer);

        if (is_starting) {
            var allocator = a.allocator();
            const bg = try style.background.toVTEscape(allocator, .background);
            defer allocator.free(bg);
            const fg = try style.foreground.toVTEscape(allocator, .foreground);
            defer allocator.free(fg);

            try output_stream.writer().print("\x1b[{};{};{}m{s}{s}", .{
                @as(u8, if (style.bold) 1 else 22),
                @as(u8, if (style.underline) 4 else 24),
                @as(u8, if (style.italic) 3 else 23),
                bg,
                fg,
            });
        }
    }

    fn vtMoveAbsolute(self: *Self, row: usize, col: usize, output_stream: anytype) !void {
        _ = self;
        _ = try output_stream.writer().print("\x1b[{d};{d}H", .{ row, col });
    }

    fn vtClearToEndOfLine(self: *Self, output_stream: anytype) !void {
        _ = self;
        _ = try output_stream.write("\x1b[K");
    }

    fn vtClearLines(self: *Self, above: usize, below: usize, output_stream: anytype) !void {
        if (above + below == 0) {
            return self.clearLine();
        }

        // Go down below lines...
        var writer = output_stream.writer();
        if (below > 0) {
            try writer.print("\x1b[{d}B", .{below});
        }

        // ...and clear lines going up.
        for (0..above + below) |i| {
            try writer.print("\x1b[2K", .{});
            if (i != 1) {
                try writer.print("\x1b[A", .{});
            }
        }

        // Go back down.
        if (above > 0) {
            try writer.print("\x1b[{d}B", .{above});
        }
    }

    fn tryUpdateOnce(self: *Self) !void {
        try self.handleReadEvent();

        if (self.always_refresh) {
            self.refresh_needed = true;
        }

        try self.refreshDisplay();

        if (self.finished) {
            try self.reallyQuitEventLoop();
        }
    }

    fn handleReadEvent(self: *Self) !void {
        if (self.prohibit_input_processing) {
            self.have_unprocessed_read_event = true;
            return;
        }

        self.prohibit_input_processing = true;
        defer self.prohibit_input_processing = false;

        var keybuf = [_]u8{0} ** 32;

        var stdin = SystemCapabilities.getStdIn();

        if (self.incomplete_data.container.items.len == 0) {
            const nread = stdin.read(&keybuf) catch |err| {
                // Zig eats EINTR, so we'll have to delay resize handling until the next read.
                self.finished = true;
                self.input_error = toWrapped(Error, err);
                return;
            };

            if (nread == 0) {
                self.input_error = toWrapped(Error, error.Empty);
                self.finished = true;
                return;
            }

            try self.incomplete_data.container.appendSlice(keybuf[0..nread]);
        }

        var available_bytes = self.incomplete_data.container.items.len;

        var reverse_tab = false;

        // Discard starting bytes until they make sense as utf-8.
        var valid_bytes: usize = 0;
        while (available_bytes > 0) {
            valid_bytes = utf8ValidRange(self.incomplete_data.container.items[0..available_bytes]);
            if (valid_bytes > 0) {
                break;
            }
            _ = self.incomplete_data.container.orderedRemove(0);
            available_bytes -= 1;
        }

        var input_view = std.unicode.Utf8View.initUnchecked(self.incomplete_data.container.items[0..valid_bytes]);
        var consumed_code_points: usize = 0;

        // FIXME: These are leaked, we have no way to free them.
        const csi = struct {
            var parameter_bytes: std.ArrayList(u8) = undefined;
            var intermediate_bytes: std.ArrayList(u8) = undefined;
            var initialized = false;
        };

        if (!csi.initialized) {
            csi.intermediate_bytes = std.ArrayList(u8).init(self.allocator);
            csi.parameter_bytes = std.ArrayList(u8).init(self.allocator);
            csi.initialized = true;
        }

        var csi_parameters = std.ArrayList(u32).init(self.allocator);
        defer csi_parameters.deinit();

        var csi_final: u8 = 0;

        var input_it = input_view.iterator();
        while (input_it.nextCodepoint()) |code_point| {
            if (self.finished) {
                break;
            }

            consumed_code_points += 1;
            if (code_point == 0) {
                continue;
            }

            switch (self.input_state) {
                .GotEscape => switch (code_point) {
                    '[' => {
                        self.input_state = .CSIExpectParameter;
                        continue;
                    },
                    else => {
                        try self.callback_machine.keyPressed(self, Key{ .code_point = code_point, .modifiers = .Alt });
                        self.input_state = .Free;
                        continue;
                    },
                },
                .CSIExpectParameter, .CSIExpectIntermediate, .CSIExpectFinal => {
                    if (self.input_state == .CSIExpectParameter) {
                        if (code_point >= 0x30 and code_point <= 0x3f) { // '0123456789:;<=>?'
                            try csi.parameter_bytes.append(@intCast(code_point));
                            continue;
                        }
                        self.input_state = .CSIExpectIntermediate;
                    }
                    if (self.input_state == .CSIExpectIntermediate) {
                        if (code_point >= 0x20 and code_point <= 0x2f) { // ' !"#$%&\'()*+,-./'
                            try csi.intermediate_bytes.append(@intCast(code_point));
                            continue;
                        }
                        self.input_state = .CSIExpectFinal;
                    }
                    if (self.input_state == .CSIExpectFinal) {
                        self.input_state = self.previous_free_state;
                        const is_in_paste = self.input_state == .Paste;
                        var it = std.mem.splitScalar(u8, csi.parameter_bytes.items, ';');
                        while (it.next()) |parameter| {
                            if (std.fmt.parseInt(u8, parameter, 10) catch null) |value| {
                                try csi_parameters.append(value);
                            } else {
                                try csi_parameters.append(0);
                            }
                        }
                        var param1: u32 = 0;
                        var param2: u32 = 0;
                        if (csi_parameters.items.len >= 1) {
                            param1 = csi_parameters.items[0];
                        }
                        if (csi_parameters.items.len >= 2) {
                            param2 = csi_parameters.items[1];
                        }

                        var modifiers: CSIMod = .None;
                        if (param2 != 0) {
                            modifiers = @enumFromInt(@as(u8, @intCast(param2 - 1)));
                        }

                        if (is_in_paste and code_point != '~' and param1 != 201) {
                            // The only valid escape to process in paste mode is the stop-paste sequence.
                            // so treat everything else as part of the pasted data.
                            self.insertCodePoint(0x1b);
                            self.insertCodePoint('[');
                            self.insertString(csi.parameter_bytes.items);
                            self.insertString(csi.intermediate_bytes.items);
                            self.insertCodePoint(code_point);
                            continue;
                        }
                        if (!(code_point >= 0x40 and code_point <= 0x7f)) {
                            logger.debug("Invalid CSI: {x:02} ({c})", .{ code_point, @as(u8, @intCast(code_point)) });
                            continue;
                        }

                        csi_final = @intCast(code_point);
                        csi_parameters.clearAndFree();
                        csi.parameter_bytes.clearAndFree();
                        csi.intermediate_bytes.clearAndFree();
                        csi.initialized = false;
                        csi.parameter_bytes.deinit();
                        csi.intermediate_bytes.deinit();

                        if (csi_final == 'Z') {
                            // 'reverse tab'
                            reverse_tab = true;
                            break;
                        }

                        try self.cleanupSuggestions();

                        switch (csi_final) {
                            'A' => { // ^[[A: arrow up
                                _ = self.searchBackwards();
                                continue;
                            },
                            'B' => { // ^[[B: arrow down
                                _ = self.searchForwards();
                                continue;
                            },
                            'D' => { // ^[[D: arrow left
                                if (modifiers == .Alt or modifiers == .Ctrl) {
                                    _ = self.cursorLeftWord();
                                } else {
                                    _ = self.cursorLeftCharacter();
                                }
                                continue;
                            },
                            'C' => { // ^[[C: arrow right
                                if (modifiers == .Alt or modifiers == .Ctrl) {
                                    _ = self.cursorRightWord();
                                } else {
                                    _ = self.cursorRightCharacter();
                                }
                                continue;
                            },
                            'H' => { // ^[[H: home
                                // TODO: self.goHome();
                                continue;
                            },
                            'F' => { // ^[[F: end
                                // TODO: self.goEnd();
                                continue;
                            },
                            127 => {
                                if (modifiers == .Ctrl) {
                                    // TODO: self.eraseAlnumWordBackwards();
                                } else {
                                    _ = self.eraseCharacterBackwards();
                                }
                                continue;
                            },
                            '~' => {
                                if (param1 == 3) { // ^[[3~: delete
                                    if (modifiers == .Ctrl) {
                                        // TODO: self.eraseAlnumWordForwards();
                                    } else {
                                        _ = self.eraseCharacterForwards();
                                    }
                                    self.search_offset = 0;
                                    continue;
                                }
                                if (self.configuration.enable_bracketed_paste) {
                                    // ^[[200~: start bracketed paste
                                    // ^[[201~: end bracketed paste
                                    if (!is_in_paste and param1 == 200) {
                                        self.input_state = .Paste;
                                        continue;
                                    }
                                    if (is_in_paste and param1 == 201) {
                                        self.input_state = .Free;
                                        if (self.on.paste) |*cb| {
                                            cb.f(cb.context, self.paste_buffer.container.items);
                                            self.paste_buffer.container.clearRetainingCapacity();
                                        }
                                        if (self.paste_buffer.size() > 0) {
                                            self.insertUtf32(self.paste_buffer.container.items);
                                        }
                                        continue;
                                    }
                                }
                                logger.debug("Unhandled '~': {}", .{param1});
                                continue;
                            },
                            else => {
                                logger.debug("Unhandled final: {x:02} ({c})", .{ code_point, @as(u8, @intCast(code_point)) });
                                continue;
                            },
                        }
                        unreachable;
                    }
                },
                .Verbatim => {
                    self.input_state = .Free;
                    // Verbatim mode will bypass all mechanisms and just insert the code point.
                    self.insertCodePoint(code_point);
                    continue;
                },
                .Paste => {
                    if (code_point == 27) {
                        self.previous_free_state = .Paste;
                        self.input_state = .GotEscape;
                        continue;
                    }

                    if (self.on.paste) |_| {
                        try self.paste_buffer.container.append(code_point);
                    } else {
                        self.insertCodePoint(code_point);
                    }
                    continue;
                },
                .Free => {
                    self.previous_free_state = .Free;
                    if (code_point == 27) {
                        try self.callback_machine.keyPressed(self, Key{ .code_point = code_point });
                        // Note that this should also deal with explicitly registered keys
                        // that would otherwise be interpreted as escapes.
                        if (self.callback_machine.shouldProcessLastPressedKey()) {
                            self.input_state = .GotEscape;
                        }
                        continue;
                    }
                    if (code_point == 22) { // ^v
                        try self.callback_machine.keyPressed(self, Key{ .code_point = code_point });
                        if (self.callback_machine.shouldProcessLastPressedKey()) {
                            self.input_state = .Verbatim;
                        }
                        continue;
                    }
                },
            }

            // There are no sequences past this point, so short of 'tab', we will want to cleanup the suggestions.
            var should_perform_suggestion_cleanup = true;
            defer if (should_perform_suggestion_cleanup) {
                self.cleanupSuggestions() catch {};
            };

            // Normally ^D. `stty eof \^n` can change it to ^N (or something else).
            // Process this here since the keybinds might override its behavior.
            // This only applies when the buffer is empty. at any other time, the behavior should be configurable.
            if (code_point == getTermiosCC(self.termios, V.EOF) and self.buffer.container.items.len == 0) {
                _ = self.finishEdit();
                continue;
            }

            try self.callback_machine.keyPressed(self, Key{ .code_point = code_point });
            if (!self.callback_machine.shouldProcessLastPressedKey()) {
                continue;
            }

            self.search_offset = 0; // reset search offset on any key

            if (code_point == '\t' or reverse_tab) {
                should_perform_suggestion_cleanup = false;

                // TODO: on_tab_complete
                continue;
            }

            // If we got here, manually cleanup the suggestions and then insert the new code point.
            self.remembered_suggestion_static_data.container.clearRetainingCapacity();
            should_perform_suggestion_cleanup = false;
            try self.cleanupSuggestions();
            self.insertCodePoint(code_point);
        }

        if (input_it.i == self.incomplete_data.container.items.len) {
            self.incomplete_data.container.clearAndFree();
        } else {
            for (input_it.i..self.incomplete_data.container.items.len) |_| {
                _ = self.incomplete_data.container.orderedRemove(input_it.i);
            }
        }

        if (self.incomplete_data.container.items.len != 0 and !self.finished) {
            try self.event_loop.loopAction(.Retry);
        }
    }

    fn handleResizeEvent(self: *Self, reset_origin: bool) !void {
        self.has_origin_reset_scheduled = false;
        if (reset_origin and !self.setOrigin(false)) {
            self.has_origin_reset_scheduled = true;
            try self.event_loop.deferredInvoke(.{ .HandleResizeEvent = true });
            return;
        }

        self.setOriginValues(self.origin_row, 1);

        {
            var stream = std.io.bufferedWriter(SystemCapabilities.getStdErr().writer());

            try self.repositionCursor(&stream, true);
            // FIXME: suggestion_display.redisplay();
            try self.repositionCursor(&stream, false);

            try stream.flush();
        }

        if (self.is_searching) {
            try self.search_editor.?.resized();
        }
    }

    fn ensureFreeLinesFromOrigin(self: *Self, count: usize) void {
        _ = self;
        _ = count;
    }

    fn vtDSR(self: *Self) ![2]usize {
        var buf = [_]u8{0} ** 32;
        var more_junk_to_read = false;
        var stdin = SystemCapabilities.getStdIn();
        var pollfds = [1]SystemCapabilities.pollfd{undefined};
        {
            var pollfd: SystemCapabilities.pollfd = undefined;
            SystemCapabilities.setPollFd(&pollfd, stdin.handle);
            pollfd.events = SystemCapabilities.POLL_IN;
            pollfd.revents = 0;
            pollfds[0] = pollfd;
        }

        while (true) {
            more_junk_to_read = false;
            const rc = SystemCapabilities.poll(&pollfds, 1, 0);
            if (rc == 1 and pollfds[0].revents & SystemCapabilities.POLL_IN != 0) {
                const nread = stdin.read(&buf) catch |err| {
                    self.finished = true;
                    self.input_error = toWrapped(Error, err);
                    return error.ReadFailure;
                };
                if (nread == 0) {
                    break;
                }
                try self.incomplete_data.container.appendSlice(buf[0..nread]);
                more_junk_to_read = true;
            }
            if (!more_junk_to_read) {
                break;
            }
        }

        if (self.input_error) |err| {
            return fromWrapped(err);
        }

        var stderr = SystemCapabilities.getStdErr();
        _ = try stderr.write("\x1b[6n");

        var state: enum {
            Free,
            SawEsc,
            SawBracket,
            InFirstCoordinate,
            SawSemicolon,
            InSecondCoordinate,
            SawR,
        } = .Free;
        var has_error = false;
        var coordinate_buffer = [8]u8{ 0, 0, 0, 0, 0, 0, 0, 0 };
        var coordinate_length: usize = 0;
        var row: usize = 0;
        var column: usize = 0;

        while (state != .SawR) {
            var b = [1]u8{0};
            const length = try stdin.read(&b);
            if (length == 0) {
                logger.debug("Got EOF while reading DSR response", .{});
                return error.Empty;
            }

            const c = b[0];

            switch (state) {
                .Free => {
                    if (c == 0x1b) {
                        state = .SawEsc;
                        continue;
                    }
                    try self.incomplete_data.container.append(c);
                },
                .SawEsc => {
                    if (c == '[') {
                        state = .SawBracket;
                        continue;
                    }
                    try self.incomplete_data.container.append(c);
                    state = .Free;
                },
                .SawBracket => {
                    if (c >= '0' and c <= '9') {
                        state = .InFirstCoordinate;
                        coordinate_buffer[0] = c;
                        coordinate_length = 1;
                        continue;
                    }
                    try self.incomplete_data.container.append(c);
                    state = .Free;
                },
                .InFirstCoordinate => {
                    if (c >= '0' and c <= '9') {
                        if (coordinate_length < coordinate_buffer.len) {
                            coordinate_buffer[coordinate_length] = c;
                            coordinate_length += 1;
                        }
                        continue;
                    }
                    if (c == ';') {
                        state = .SawSemicolon;
                        // parse the first coordinate
                        row = std.fmt.parseInt(u8, coordinate_buffer[0..coordinate_length], 10) catch v: {
                            has_error = true;
                            break :v 1;
                        };
                        coordinate_length = 0;
                        continue;
                    }
                    try self.incomplete_data.container.append(c);
                    state = .Free;
                },
                .SawSemicolon => {
                    if (c >= '0' and c <= '9') {
                        state = .InSecondCoordinate;
                        coordinate_buffer[0] = c;
                        coordinate_length = 1;
                        continue;
                    }
                    try self.incomplete_data.container.append(c);
                    state = .Free;
                },
                .InSecondCoordinate => {
                    if (c >= '0' and c <= '9') {
                        if (coordinate_length < coordinate_buffer.len) {
                            coordinate_buffer[coordinate_length] = c;
                            coordinate_length += 1;
                        }
                        continue;
                    }
                    if (c == 'R') {
                        // parse the second coordinate
                        state = .SawR;
                        column = std.fmt.parseInt(u8, coordinate_buffer[0..coordinate_length], 10) catch v: {
                            has_error = true;
                            break :v 1;
                        };
                        continue;
                    }
                    try self.incomplete_data.container.append(c);
                },
                .SawR => {
                    unreachable;
                },
            }
        }

        if (has_error) {
            logger.debug("Couldn't parse DSR response", .{});
        }

        return [2]usize{ row, column };
    }

    fn removeAtIndex(self: *Self, index: usize) void {
        const c = self.buffer.container.orderedRemove(index);
        if (c == '\n') {
            self.extra_forward_lines += 1;
        }
        self.chars_touched_in_the_middle += 1;
    }

    fn reset(self: *Self) !void {
        try self.cached_buffer_metrics.reset();
        self.cached_prompt_valid = false;
        self.cursor = 0;
        self.drawn_cursor = 0;
        self.inline_search_cursor = 0;
        self.search_offset = 0;
        self.search_offset_state = .Unbiased;
        self.old_prompt_metrics = self.cached_prompt_metrics;
        self.origin_row = 0;
        self.origin_column = 0;
        self.prompt_lines_at_suggestion_initiation = 0;
        self.refresh_needed = true;
        self.input_error = null;
        self.returned_line = &[0]u8{};
        self.chars_touched_in_the_middle = 0;
        self.drawn_end_of_line_offset = 0;
        self.current_spans.deinit();
        self.current_spans = DrawnSpans.init(self.allocator);
        self.paste_buffer.container.clearAndFree();
    }

    fn search(self: *Self, phrase: []const u8, allow_empty: bool, from_beginning: bool) bool {
        var last_matching_offset: i32 = -1;
        var found: bool = false;

        if (allow_empty or phrase.len > 0) {
            var search_offset = self.search_offset;
            var i = self.history_cursor;
            while (i > 0) : (i -= 1) {
                const entry = self.history.container.items[i - 1];
                // Dear zig fmt, please understand that sometimes you have to break lines.
                const contains = if (from_beginning) r: {
                    break :r std.mem.startsWith(u8, entry.entry.container.items, phrase);
                } else r: {
                    break :r std.mem.containsAtLeast(u8, entry.entry.container.items, 1, phrase);
                };
                if (contains) {
                    last_matching_offset = @as(i32, @intCast(i)) - 1;
                    if (search_offset == 0) {
                        found = true;
                        break;
                    }
                    if (search_offset > 0) {
                        search_offset -= 1;
                    }
                }
            }

            if (!found) {
                _ = SystemCapabilities.getStdErr().write("\x07") catch 0;
            }
        }

        if (found) {
            // We're gonna clear the buffer, so mark the entire thing touched.
            self.chars_touched_in_the_middle = self.buffer.container.items.len;
            self.buffer.container.clearRetainingCapacity();
            self.cursor = 0;
            self.insertString(self.history.container.items[@intCast(last_matching_offset)].entry.container.items);
            // Always needed.
            self.refresh_needed = true;
        }

        return found;
    }

    fn endSearch(self: *Self) void {
        self.is_searching = false;
        self.refresh_needed = true;
        self.search_offset = 0;
        if (self.reset_buffer_on_search_end) {
            self.buffer.container.clearRetainingCapacity();
            self.insertUtf32(self.pre_search_buffer.container.items);
            self.cursor = self.pre_search_cursor;
        }
        self.reset_buffer_on_search_end = true;
        if (self.search_editor) |e| {
            e.deinit();
        }
        self.search_editor = null;
    }

    fn findApplicableStyle(self: *Self, index: usize) Style {
        var style = Style.resetStyle();
        var it = self.current_spans.starting.container.iterator();
        while (it.next()) |entry| {
            unifyStylesInto(entry, index, &style);
        }

        return style;
    }

    fn unifyStylesInto(styles: std.AutoHashMap(usize, AutoHashMap(usize, Style)).Entry, offset: usize, target: *Style) void {
        if (styles.key_ptr.* >= offset) {
            return;
        }

        var it = styles.value_ptr.container.unmanaged.iterator();
        while (it.next()) |entry| {
            if (entry.key_ptr.* <= offset) {
                return;
            }
            target.unifyWith(entry.value_ptr.*, true);
        }
    }

    fn refreshDisplay(self: *Self) !void {
        if (self.was_interrupted) {
            self.was_interrupted = false;
            return;
        }

        var buffered_output = std.io.bufferedWriter(SystemCapabilities.getStdErr().writer());
        defer {
            self.shown_lines = self.currentPromptMetrics().linesWithAddition(self.cached_buffer_metrics, self.num_columns);
            _ = buffered_output.flush() catch {};
        }

        const has_cleaned_up = false;
        // Someone changed the window size, figure it out
        // and react to it. We might need to redraw.
        if (self.was_resized) {
            if (self.previous_num_columns != self.num_columns) {
                // We need to cleanup and redo everything.
                self.cached_prompt_valid = false;
                self.refresh_needed = true;
                std.mem.swap(usize, &self.previous_num_columns, &self.num_columns);
                self.recalculateOrigin();
                try self.cleanup();
                std.mem.swap(usize, &self.previous_num_columns, &self.num_columns);
                self.refresh_needed = true;
            }
            self.was_resized = false;
        }

        // We might be at the last line, and more than one line;
        // Refreshing the display will cause the terminal to scroll,
        // so note that fact and bring the origin up, making sure to
        // reserve the space for however many lines we move it up.
        const current_num_lines = self.numLines();
        if (self.origin_row + current_num_lines > self.num_lines) {
            if (current_num_lines > self.num_lines) {
                for (0..self.num_lines) |_| {
                    _ = try buffered_output.write("\n");
                }
                self.origin_row = 0;
            } else {
                const old_origin_row = self.origin_row;
                self.origin_row = self.num_lines - current_num_lines + 1;
                for (0..old_origin_row - self.origin_row) |_| {
                    _ = try buffered_output.write("\n");
                }
            }
        }

        // Do not call hook on pure cursor movements.
        if (self.cached_prompt_valid and !self.refresh_needed and self.pending_chars.container.items.len == 0) {
            try self.repositionCursor(&buffered_output, false);
            self.cached_buffer_metrics.deinit();
            self.cached_buffer_metrics = try self.actualRenderedUnicodeStringMetrics(self.buffer.container.items);
            self.drawn_end_of_line_offset = self.buffer.size();
            return;
        }

        if (self.on.display_refresh) |*cb| {
            cb.f(cb.context);
        }

        var empty_styles = AutoHashMap(usize, Style).init(self.allocator);
        defer empty_styles.deinit();

        if (self.cached_prompt_valid) {
            if (!self.refresh_needed and self.cursor == self.buffer.size()) {
                // Just write the characters out and continue,
                // no need to refresh anything else.
                for (self.drawn_cursor..self.buffer.size()) |i| {
                    try self.applyStyles(&empty_styles, &buffered_output, i);
                    try self.printCharacterAt(i, &buffered_output);
                }
                try self.vtApplyStyle(Style.resetStyle(), &buffered_output, true);
                self.pending_chars.container.clearAndFree();
                self.drawn_cursor = self.cursor;
                self.drawn_end_of_line_offset = self.buffer.size();
                self.cached_buffer_metrics.deinit();
                self.cached_buffer_metrics = try self.actualRenderedUnicodeStringMetrics(self.buffer.container.items);
                return;
            }
        }

        // Ouch, reflow entire line.
        if (!has_cleaned_up) {
            try self.cleanup();
        }

        try self.vtMoveAbsolute(self.origin_row, self.origin_column, &buffered_output);

        _ = try buffered_output.write(self.new_prompt.container.items);

        try self.vtClearToEndOfLine(&buffered_output);

        for (0..self.buffer.size()) |i| {
            try self.applyStyles(&empty_styles, &buffered_output, i);
            try self.printCharacterAt(i, &buffered_output);
        }

        try self.vtApplyStyle(Style.resetStyle(), &buffered_output, true);

        self.pending_chars.container.clearAndFree();
        self.refresh_needed = false;
        self.cached_buffer_metrics.deinit();
        self.cached_buffer_metrics = try self.actualRenderedUnicodeStringMetrics(self.buffer.container.items);
        self.chars_touched_in_the_middle = 0;
        self.drawn_end_of_line_offset = self.buffer.size();
        self.cached_prompt_valid = true;

        try self.repositionCursor(&buffered_output, false);
    }

    pub fn setHandler(self: *Self, handler: anytype) void {
        const T = @TypeOf(handler);
        if (@typeInfo(T) != .pointer) {
            @compileError("Handler must be a pointer type");
        }

        const InnerT = @TypeOf(handler.*);

        inline for (@typeInfo(InnerT).@"struct".decls) |decl| {
            const h = &@field(self.on, decl.name);
            h.* = .{
                .f = &@TypeOf(h.*.?).makeHandler(T, InnerT, decl.name).theHandler,
                .context = handler,
            };
        }
    }

    fn applyStyles(self: *Self, empty_styles: *AutoHashMap(usize, Style), output_stream: anytype, index: usize) !void {
        const HM = AutoHashMap(usize, AutoHashMap(usize, Style));
        var ends = (self.current_spans.ending.container.getEntry(index) orelse HM.Entry{
            .value_ptr = empty_styles,
            .key_ptr = undefined,
        }).value_ptr;

        var starts = (self.current_spans.starting.container.getEntry(index) orelse HM.Entry{
            .value_ptr = empty_styles,
            .key_ptr = undefined,
        }).value_ptr;

        if (ends.container.count() > 0) {
            var style = Style{};

            var it = ends.container.unmanaged.iterator();
            while (it.next()) |entry| {
                style.unifyWith(entry.value_ptr.*, false);
            }

            // Disable any style that should be turned off.
            try self.vtApplyStyle(style, output_stream, false);

            // Reapply styles for overlapping spans that include this one.
            style = self.findApplicableStyle(index);
            try self.vtApplyStyle(style, output_stream, true);
        }

        if (starts.container.count() > 0) {
            var style = Style{};
            var it = starts.container.unmanaged.iterator();
            while (it.next()) |entry| {
                style.unifyWith(entry.value_ptr.*, true);
            }

            // Set new styles.
            try self.vtApplyStyle(style, output_stream, true);
        }
    }

    fn printCharacterAt(self: *Self, index: usize, output_stream: anytype) !void {
        return self.printSingleCharacter(self.buffer.container.items[index], output_stream);
    }

    fn printSingleCharacter(self: *Self, code_point: u32, output_stream: anytype) !void {
        var buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        const should_print_masked = isAsciiControl(code_point) and code_point != '\n';
        const should_print_caret = code_point < 64 and should_print_masked;
        if (should_print_caret) {
            try buffer.append('^');
            try buffer.append(@intCast(code_point + 64));
        } else {
            const c: u21 = @intCast(code_point);
            const length = try std.unicode.utf8CodepointSequenceLength(c);
            try buffer.appendNTimes(0, length);
            _ = try std.unicode.utf8Encode(c, buffer.items[buffer.items.len - length .. buffer.items.len]);
        }

        _ = try output_stream.write(buffer.items);
    }

    fn cleanup(self: *Self) !void {
        const current_buffer_metrics = try self.actualRenderedUnicodeStringMetrics(self.buffer.container.items);
        self.cached_buffer_metrics.deinit();
        self.cached_buffer_metrics = current_buffer_metrics;
        const new_lines = self.currentPromptMetrics().linesWithAddition(current_buffer_metrics, self.num_columns);
        const shown_lines = self.shown_lines;
        if (new_lines < shown_lines) {
            self.extra_forward_lines = @max(shown_lines - new_lines, self.extra_forward_lines);
        }

        var stderr = SystemCapabilities.getStdErr();
        try self.repositionCursor(&stderr, true);
        const current_line = self.numLines();
        try self.vtClearLines(current_line, self.extra_forward_lines, &stderr);
        self.extra_forward_lines = 0;
        try self.repositionCursor(&stderr, false);
    }

    fn cleanupSuggestions(self: *Self) !void {
        _ = self;
    }

    fn reallyQuitEventLoop(self: *Self) !void {
        self.finished = false;
        var stderr = SystemCapabilities.getStdErr();
        var stream = stderr.writer();
        try self.repositionCursor(&stderr, true);
        _ = try stream.write("\n");

        const str = try self.getBufferedLine();
        self.buffer.container.clearAndFree();
        self.chars_touched_in_the_middle = 0;
        self.is_editing = false;
        self.returned_line = str;

        if (self.initialized) {
            self.restore() catch {};
        }

        try self.event_loop.loopAction(.Exit);
    }

    fn restore(self: *Self) !void {
        if (!self.initialized) unreachable;

        try setTermios(self.default_termios);
        self.initialized = false;
        if (self.configuration.enable_bracketed_paste) {
            var stderr = SystemCapabilities.getStdErr();
            try stderr.writeAll("\x1b[?2004l");
        }
    }

    fn currentPromptMetrics(self: *Self) StringMetrics {
        if (!self.cached_prompt_valid) {
            return self.old_prompt_metrics;
        }

        return self.cached_prompt_metrics;
    }

    fn numLines(self: *Self) usize {
        return self.currentPromptMetrics().linesWithAddition(self.cached_buffer_metrics, self.num_columns);
    }

    fn cursorLine(self: *Self) !usize {
        var cursor = self.drawn_cursor;
        if (cursor > self.cursor) {
            cursor = self.cursor;
        }
        var metrics = try self.actualRenderedUnicodeStringMetrics(self.buffer.container.items[0..cursor]);
        defer metrics.deinit();
        return self.currentPromptMetrics().linesWithAddition(metrics, self.num_columns);
    }

    fn offsetInLine(self: *Self) !usize {
        var cursor = self.drawn_cursor;
        if (cursor > self.cursor) {
            cursor = self.cursor;
        }
        var metrics = try self.actualRenderedUnicodeStringMetrics(self.buffer.container.items[0..cursor]);
        defer metrics.deinit();
        return self.currentPromptMetrics().offsetWithAddition(metrics, self.num_columns);
    }

    fn setOrigin(self: *Self, quit_on_error: bool) bool {
        const position = self.vtDSR() catch |err| {
            if (quit_on_error) {
                self.input_error = toWrapped(Error, err);
                _ = self.finish();
            }
            return false;
        };
        self.setOriginValues(position[0], position[1]);
        return true;
    }

    fn setOriginValues(self: *Self, row: usize, column: usize) void {
        self.origin_row = row;
        self.origin_column = column;
        if (self.suggestion_display) |*display| {
            _ = display;
            // FIXME: display.setOriginValues(row, column);
        }
    }

    fn recalculateOrigin(self: *Self) void {
        _ = self;
    }

    fn repositionCursor(self: *Self, output_stream: anytype, to_end: bool) !void {
        var cursor = self.cursor;
        const saved_cursor = cursor;
        if (to_end) {
            cursor = self.buffer.size();
        }

        self.cursor = cursor;
        self.drawn_cursor = cursor;

        const line = try self.cursorLine() - 1;
        const column = try self.offsetInLine();

        self.ensureFreeLinesFromOrigin(line);

        try self.vtMoveAbsolute(line + self.origin_row, column + self.origin_column, output_stream);

        self.cursor = saved_cursor;
    }

    const CodePointRange = struct {
        start: u32,
        end: u32,
    };

    fn byteOffsetRangeToCodePointOffsetRange(
        self: *Self,
        byte_start: usize,
        byte_end: usize,
        code_point_scan_offset: usize,
        reverse: bool,
    ) CodePointRange {
        _ = self;
        _ = byte_start;
        _ = byte_end;
        _ = code_point_scan_offset;
        _ = reverse;
        return undefined;
    }

    fn getTerminalSize(self: *Self) void {
        self.num_columns = 80;
        self.num_lines = 24;
        if (!is_windows) {
            const ws = SystemCapabilities.getWinsize(SystemCapabilities.getStdIn().handle) catch {
                return;
            };
            self.num_columns = ws.col;
            self.num_lines = ws.row;
        }
    }

    pub fn goEnd(self: *Self) bool {
        self.cursor = self.buffer.size();
        self.inline_search_cursor = self.cursor;
        self.search_offset = 0;
        return false;
    }

    pub fn goHome(self: *Self) bool {
        self.cursor = 0;
        self.inline_search_cursor = self.cursor;
        self.search_offset = 0;
        return false;
    }

    pub fn clearScreen(self: *Self) bool {
        var stderr = SystemCapabilities.getStdErr();
        _ = stderr.write("\x1b[3J\x1b[H\x1b[2J") catch 0;

        self.vtMoveAbsolute(1, 1, &stderr) catch {};
        self.setOriginValues(1, 1);
        self.refresh_needed = true;
        self.cached_prompt_valid = false;
        return false;
    }

    pub fn eraseCharacterBackwards(self: *Self) bool {
        if (self.is_searching) {
            return false;
        }

        if (self.cursor == 0) {
            _ = SystemCapabilities.getStdErr().write("\x07") catch 0; // \a BEL
            return false;
        }

        self.removeAtIndex(self.cursor - 1);
        self.cursor -= 1;
        self.inline_search_cursor = self.cursor;
        self.refresh_needed = true;
        return false;
    }

    pub fn eraseCharacterForwards(self: *Self) bool {
        if (self.is_searching) {
            return false;
        }

        if (self.cursor == self.buffer.size()) {
            _ = SystemCapabilities.getStdErr().write("\x07") catch 0; // \a BEL
            return false;
        }

        self.removeAtIndex(self.cursor);
        self.refresh_needed = true;
        return false;
    }

    pub fn cursorLeftCharacter(self: *Self) bool {
        if (self.cursor == 0) {
            return false;
        }

        self.cursor -= 1;
        self.inline_search_cursor = self.cursor;
        return false;
    }

    pub fn cursorRightCharacter(self: *Self) bool {
        if (self.cursor == self.buffer.size()) {
            return false;
        }

        self.cursor += 1;
        self.inline_search_cursor = self.cursor;
        return false;
    }

    pub fn searchForwards(self: *Self) bool {
        const p = sane.checkpoint(&self.inline_search_cursor);
        defer p.restore();

        var builder = sane.StringBuilder.init(self.allocator);
        defer builder.deinit();

        builder.appendUtf32Slice(self.buffer.container.items[0..self.inline_search_cursor]) catch return false;
        const search_phrase = builder.toSlice();

        if (self.search_offset_state == .Backwards) {
            if (self.search_offset > 0) {
                self.search_offset -= 1;
            }
        }

        if (self.search_offset > 0) {
            var p1 = sane.checkpoint(&self.search_offset);
            defer p1.restore();

            self.search_offset -= 1;
            if (self.search(search_phrase, true, true)) {
                self.search_offset_state = .Forwards;
                p1.v = self.search_offset;
            } else {
                self.search_offset_state = .Unbiased;
            }
        } else {
            self.search_offset_state = .Unbiased;
            self.chars_touched_in_the_middle = self.buffer.size();
            self.cursor = 0;
            self.buffer.container.clearRetainingCapacity();
            self.insertString(search_phrase);
            self.refresh_needed = true;
        }

        return false;
    }

    pub fn searchBackwards(self: *Self) bool {
        const p = sane.checkpoint(&self.inline_search_cursor);
        defer p.restore();

        var builder = sane.StringBuilder.init(self.allocator);
        defer builder.deinit();

        builder.appendUtf32Slice(self.buffer.container.items[0..self.inline_search_cursor]) catch return false;
        const search_phrase = builder.toSlice();

        if (self.search_offset_state == .Forwards) {
            self.search_offset += 1;
        }

        if (self.search(search_phrase, true, true)) {
            self.search_offset_state = .Backwards;
            self.search_offset += 1;
        } else {
            self.search_offset_state = .Unbiased;
            if (self.search_offset > 0) {
                self.search_offset -= 1;
            }
        }

        return false;
    }

    pub fn finishEdit(self: *Self) bool {
        self.prohibitInput();
        defer self.allowInput();

        _ = SystemCapabilities.getStdErr().write("<EOF>\n") catch 0;
        if (!self.always_refresh) {
            self.input_error = toWrapped(Error, error.Eof);
            _ = self.finish();
            self.reallyQuitEventLoop() catch {};
        }
        return false;
    }

    pub fn eraseToEnd(self: *Self) bool {
        if (self.cursor == self.buffer.size()) {
            return false;
        }

        while (self.cursor < self.buffer.size()) {
            _ = self.eraseCharacterForwards();
        }

        return false;
    }

    pub fn transposeCharacters(self: *Self) bool {
        if (self.cursor > 0 and self.buffer.size() >= 2) {
            if (self.cursor < self.buffer.size()) {
                self.cursor += 1;
            }

            std.mem.swap(u32, &self.buffer.container.items[self.cursor - 1], &self.buffer.container.items[self.cursor - 2]);
            self.refresh_needed = true;
            self.chars_touched_in_the_middle += 2;
        }
        return false;
    }

    pub fn cursorLeftWord(self: *Self) bool {
        var has_seen_alnum = false;
        while (self.cursor > 0) {
            if (!isAsciiAlnum(self.buffer.container.items[self.cursor - 1])) {
                if (has_seen_alnum) {
                    break;
                }
            } else {
                has_seen_alnum = true;
            }

            self.cursor -= 1;
        }
        self.inline_search_cursor = self.cursor;
        return false;
    }

    pub fn cursorLeftNonspaceWord(self: *Self) bool {
        var has_seen_space = false;
        while (self.cursor > 0) {
            if (isAsciiSpace(self.buffer.container.items[self.cursor - 1])) {
                if (has_seen_space) {
                    break;
                }
            } else {
                has_seen_space = true;
            }

            self.cursor -= 1;
        }
        self.inline_search_cursor = self.cursor;
        return false;
    }

    pub fn cursorRightWord(self: *Self) bool {
        var has_seen_alnum = false;
        while (self.cursor < self.buffer.size()) {
            if (!isAsciiAlnum(self.buffer.container.items[self.cursor])) {
                if (has_seen_alnum) {
                    break;
                }
            } else {
                has_seen_alnum = true;
            }

            self.cursor += 1;
        }
        self.inline_search_cursor = self.cursor;
        self.search_offset = 0;
        return false;
    }

    pub fn cursorRightNonspaceWord(self: *Self) bool {
        var has_seen_space = false;
        while (self.cursor < self.buffer.size()) {
            if (isAsciiSpace(self.buffer.container.items[self.cursor])) {
                if (has_seen_space) {
                    break;
                }
            } else {
                has_seen_space = true;
            }

            self.cursor += 1;
        }
        self.inline_search_cursor = self.cursor;
        self.search_offset = 0;
        return false;
    }

    pub fn eraseAlnumWordBackwards(self: *Self) bool {
        if (self.cursor == 0) {
            return false;
        }

        var has_seen_alnum = false;
        while (self.cursor > 0) {
            if (!isAsciiAlnum(self.buffer.container.items[self.cursor - 1])) {
                if (has_seen_alnum) {
                    break;
                }
            } else {
                has_seen_alnum = true;
            }

            _ = self.eraseCharacterBackwards();
        }
        return false;
    }

    pub fn eraseAlnumWordForwards(self: *Self) bool {
        if (self.cursor == self.buffer.size()) {
            return false;
        }

        var has_seen_alnum = false;
        while (self.cursor < self.buffer.size()) {
            if (!isAsciiAlnum(self.buffer.container.items[self.cursor])) {
                if (has_seen_alnum) {
                    break;
                }
            } else {
                has_seen_alnum = true;
            }

            _ = self.eraseCharacterForwards();
        }
        return false;
    }

    pub fn eraseWordBackwards(self: *Self) bool {
        if (self.cursor == 0) {
            return false;
        }

        var has_seen_nonspace = false;
        while (self.cursor > 0) {
            if (isAsciiSpace(self.buffer.container.items[self.cursor - 1])) {
                if (has_seen_nonspace) {
                    break;
                }
            } else {
                has_seen_nonspace = true;
            }

            _ = self.eraseCharacterBackwards();
        }
        return false;
    }

    pub fn capitalizeWord(self: *Self) bool {
        self.caseChangeWord(.Capital);
        return false;
    }

    pub fn lowercaseWord(self: *Self) bool {
        self.caseChangeWord(.Lower);
        return false;
    }

    pub fn uppercaseWord(self: *Self) bool {
        self.caseChangeWord(.Upper);
        return false;
    }

    pub fn killLine(self: *Self) bool {
        if (self.cursor == 0) {
            return false;
        }

        for (0..self.cursor) |_| {
            self.removeAtIndex(0);
        }
        self.cursor = 0;
        self.inline_search_cursor = 0;
        self.refresh_needed = true;
        return false;
    }

    fn caseChangeWord(self: *Self, op: enum { Lower, Upper, Capital }) void {
        while (self.cursor < self.buffer.size() and !isAsciiAlnum(self.buffer.container.items[self.cursor])) {
            self.cursor += 1;
        }
        const start = self.cursor;
        while (self.cursor < self.buffer.size() and isAsciiAlnum(self.buffer.container.items[self.cursor])) {
            if (op == .Upper or (op == .Capital and self.cursor == start)) {
                self.buffer.container.items[self.cursor] = std.ascii.toUpper(@intCast(self.buffer.container.items[self.cursor]));
            } else {
                self.buffer.container.items[self.cursor] = std.ascii.toLower(@intCast(self.buffer.container.items[self.cursor]));
            }
            self.cursor += 1;
        }

        self.refresh_needed = true;
        self.chars_touched_in_the_middle += 1;
    }
};
