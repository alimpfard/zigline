const std = @import("std");
const Allocator = std.mem.Allocator;
const Thread = std.Thread;
const Condition = Thread.Condition;
const Mutex = Thread.Mutex;

const sane = @import("sane.zig");
const ArrayList = sane.ArrayList;
const AutoHashMap = sane.AutoHashMap;
const Queue = sane.Queue;

// FIXME: Windows :P
fn getTermios() !std.os.termios {
    return try std.os.tcgetattr(std.os.STDIN_FILENO);
}

fn setTermios(termios: std.os.termios) !void {
    try std.os.tcsetattr(std.os.STDIN_FILENO, std.os.linux.TCSA.NOW, termios);
}

fn isAsciiControl(code_point: u32) bool {
    return code_point < 0x20 or code_point == 0x7f;
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
            const v = std.mem.readIntNative(usize, s[i..][0..N]);
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
    is_empty: bool = true,
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

        pub fn isDefault(self: *@This()) bool {
            return switch (self) {
                .xterm => self.xterm == XtermColor.Unchanged,
                .rgb => self.rgb[0] == 0 and self.rgb[1] == 0 and self.rgb[2] == 0,
            };
        }
    };

    pub fn resetStyle() Self {
        return Self{};
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
            self.set_bold(true);
        }

        if (other.italic) {
            self.set_italic(true);
        }

        if (other.underline) {
            self.set_underline(true);
        }

        self.is_empty = self.is_empty and other.is_empty;
    }

    pub fn set_underline(self: *Self, underline: bool) void {
        self.underline = underline;
        self.is_empty = false;
    }

    pub fn set_bold(self: *Self, bold: bool) void {
        self.bold = bold;
        self.is_empty = false;
    }

    pub fn set_italic(self: *Self, italic: bool) void {
        self.italic = italic;
        self.is_empty = false;
    }

    pub fn set_background(self: *Self, background: Color) void {
        self.background = background;
        self.is_empty = false;
    }

    pub fn set_foreground(self: *Self, foreground: Color) void {
        self.foreground = foreground;
        self.is_empty = false;
    }
};

pub const StringMetrics = struct {
    pub const LineMetrics = struct {
        length: usize = 0,
        visible_length: usize = 0,
        bit_length: ?usize = null,

        pub fn total_length(self: *@This()) usize {
            return self.length;
        }
    };

    line_metrics: ArrayList(LineMetrics),
    total_length: usize = 0,
    max_line_length: usize = 0,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self{
            .line_metrics = ArrayList(LineMetrics).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.line_metrics.deinit();
    }

    pub fn lines_with_addition(self: *const Self, offset: Self, column_width: usize) usize {
        _ = self;
        _ = offset;
        _ = column_width;
        return 0;
    }

    pub fn offset_with_addition(self: *const Self, offset: Self, column_width: usize) usize {
        _ = self;
        _ = offset;
        _ = column_width;
        return 0;
    }

    pub fn reset(self: *Self) void {
        self.line_metrics.clearAndFree();
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
    } = .Alt,

    pub fn equals(self: *const @This(), other: @This()) bool {
        return self.code_point == other.code_point and self.modifiers == other.modifiers;
    }
};

const KeyCallbackEntry = struct {
    sequence: ArrayList(Key),
    callback: *const fn (*Editor) bool,

    pub fn init(allocator: Allocator, sequence: []const Key, f: *const fn (*Editor) bool) @This() {
        var self = @This(){
            .sequence = ArrayList(Key).init(allocator),
            .callback = f,
        };
        self.sequence.container.appendSlice(sequence) catch unreachable;
        return self;
    }

    pub fn deinit(self: *@This()) void {
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
        return Self{
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
                    self.key_callbacks.container.items[j] = inserted_entry;
                    return;
                }
            }
        }

        try self.key_callbacks.container.append(inserted_entry);
    }

    pub fn interrupted(self: *Self, editor: *Editor) void {
        self.sequence_length = 0;
        self.current_matching_keys.container.clearAndFree();
        self.should_process_this_key = self.callback([1]Key{Key{ .code_point = ctrl('C') }}, editor, true);
    }

    pub fn shouldProcessLastPressedKey(self: *Self) bool {
        return self.should_process_this_key;
    }
};
pub const HistoryEntry = struct {
    entry: ArrayList(u8),
    timestamp: u64,

    const Self = @This();

    pub fn init(allocator: Allocator, entry: []const u8) Self {
        var self = Self{
            .entry = ArrayList(u8).init(allocator),
            .timestamp = std.time.currentTime(),
        };
        self.entry.container.appendSlice(entry);
        return self;
    }

    pub fn initWithTimestamp(allocator: Allocator, entry: []const u8, timestamp: u64) Self {
        var self = Self{
            .entry = ArrayList(u8).init(allocator),
            .timestamp = timestamp,
        };
        self.entry.container.appendSlice(entry);
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.entry.deinit();
    }
};
pub const Configuration = struct {
    enable_bracketed_paste: bool = true,
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

    var writer = std.io.getStdErr().writer();
    if (row > 0) {
        try writer.print("\x1b[{d}{c}", .{ r, x_op });
    }
    if (col > 0) {
        try writer.print("\x1b[{d}{c}", .{ c, y_op });
    }
}

pub const Editor = struct {
    pub const Signal = enum {
        SIGINT,
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
    const DrawnSpans = struct {
        starting: AutoHashMap(u32, std.AutoHashMap(u32, Style)),
        ending: AutoHashMap(u32, std.AutoHashMap(u32, Style)),

        pub fn init(allocator: Allocator) @This() {
            return .{
                .starting = AutoHashMap(u32, std.AutoHashMap(u32, Style)).init(allocator),
                .ending = AutoHashMap(u32, std.AutoHashMap(u32, Style)).init(allocator),
            };
        }

        pub fn deinit(self: *@This()) void {
            self.starting.deinit();
            self.ending.deinit();
        }

        pub fn containsUpToOffset(self: *const @This(), other: @This(), offset: usize) bool {
            _ = self;
            _ = other;
            _ = offset;
            return false;
        }
    };
    const LoopExitCode = enum {
        Exit,
        Retry,
    };
    const DeferredAction = union(enum) {
        HandleResizeEvent: bool, // reset_origin
        TryUpdateOnce: u8, // single byte read from stdin
    };

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
    input_error: ?anyerror = null,
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
    termios: std.os.termios = undefined,
    default_termios: std.os.termios = undefined,
    was_interrupted: bool = false,
    previous_interrupt_was_handled_as_interrupt: bool = false,
    was_resized: bool = false,
    history: ArrayList(HistoryEntry),
    history_cursor: usize = 0,
    history_capacity: usize = 1024,
    history_dirty: bool = false,
    input_state: InputState = .Free,
    previous_free_state: InputState = .Free,
    drawn_spans: DrawnSpans,
    current_spans: DrawnSpans,
    paste_buffer: ArrayList(u32),
    initialized: bool = false,
    refresh_needed: bool = false,
    signal_handlers: [2]i32 = .{ 0, 0 },
    is_editing: bool = false,
    prohibit_input_processing: bool = false,
    have_unprocessed_read_event: bool = false,
    configuration: Configuration,
    control_thread: ?Thread = null,

    queue_cond_mutex: Mutex = .{},
    queue_condition: Condition = .{},
    loop_queue: Queue(LoopExitCode),
    deferred_action_queue: Queue(DeferredAction),
    signal_queue: Queue(Signal),

    pub fn init(allocator: Allocator, configuration: Configuration) Self {
        var self = Self{
            .allocator = allocator,
            .buffer = ArrayList(u32).init(allocator),
            .callback_machine = KeyCallbackMachine.init(allocator),
            .pre_search_buffer = ArrayList(u32).init(allocator),
            .pending_chars = ArrayList(u8).init(allocator),
            .incomplete_data = ArrayList(u8).init(allocator),
            .returned_line = &[0]u8{},
            .remembered_suggestion_static_data = ArrayList(u32).init(allocator),
            .history = ArrayList(HistoryEntry).init(allocator),
            .drawn_spans = DrawnSpans.init(allocator),
            .current_spans = DrawnSpans.init(allocator),
            .new_prompt = ArrayList(u8).init(allocator),
            .paste_buffer = ArrayList(u32).init(allocator),
            .configuration = configuration,
            .loop_queue = Queue(LoopExitCode).init(allocator),
            .deferred_action_queue = Queue(DeferredAction).init(allocator),
            .signal_queue = Queue(Signal).init(allocator),
            .cached_prompt_metrics = StringMetrics.init(allocator),
            .old_prompt_metrics = StringMetrics.init(allocator),
            .cached_buffer_metrics = StringMetrics.init(allocator),
        };

        return self;
    }

    pub fn deinit(self: *Self) void {
        if (self.control_thread) |t| {
            // FIXME: This is a hack, the thread should be cancelled.
            t.detach();
        }

        self.buffer.deinit();
        self.pre_search_buffer.deinit();
        self.pending_chars.deinit();
        self.incomplete_data.deinit();
        self.remembered_suggestion_static_data.deinit();
        self.history.deinit();
        self.new_prompt.deinit();
        self.paste_buffer.deinit();
        self.drawn_spans.deinit();
        self.current_spans.deinit();
    }

    pub fn reFetchDefaultTermios(self: *Self) void {
        _ = self;
    }

    pub fn addToHistory(self: *Self, line: []const u8) void {
        self.history.container.append(HistoryEntry.init(self.allocator, line));
    }

    pub fn loadHistory(self: *Self, path: []const u8) void {
        _ = path;
        _ = self;
    }

    pub fn saveHistory(self: *Self, path: []const u8) void {
        _ = path;
        _ = self;
    }

    pub fn stripStyles(self: *Self) void {
        self.current_spans.ending.container.clearAndFree();
        self.current_spans.starting.container.clearAndFree();
    }

    pub fn getLine(self: *Self, prompt: []const u8) ![]const u8 {
        self.queue_cond_mutex.lock();
        defer self.queue_cond_mutex.unlock();

        start: while (true) {
            try self.initialize();

            self.is_editing = true;
            const old_cols = self.num_columns;
            const old_lines = self.num_lines;
            self.getTerminalSize();

            var stderr = std.io.getStdErr();

            if (self.configuration.enable_bracketed_paste) {
                try stderr.writeAll("\x1b[?2004h");
            }

            if (self.num_columns != old_cols or self.num_lines != old_lines) {
                self.refresh_needed = true;
            }

            try self.setPrompt(prompt);
            self.reset();
            self.stripStyles();

            const prompt_lines = std.math.max(self.currentPromptMetrics().line_metrics.container.items.len, 1) - 1;
            for (0..prompt_lines) |_| {
                try stderr.writeAll("\n");
            }

            try vtMoveRelative(-@intCast(i64, prompt_lines), 0);
            _ = self.setOrigin(true);

            self.history_cursor = self.history.container.items.len;

            try self.refreshDisplay();

            self.control_thread = try Thread.spawn(.{}, Self.controlThreadMain, .{self});

            if (self.incomplete_data.container.items.len != 0) {
                try self.deferred_action_queue.enqueue(DeferredAction{ .TryUpdateOnce = self.incomplete_data.container.orderedRemove(0) });
                self.queue_condition.broadcast();
            }

            // FIXME: Install signal handlers.

            while (true) {
                self.queue_condition.wait(&self.queue_cond_mutex);

                while (!self.signal_queue.isEmpty()) {
                    switch (self.signal_queue.dequeue()) {
                        .SIGINT => {
                            self.interrupted();
                        },
                        .SIGWINCH => {
                            self.resized();
                        },
                    }
                }

                while (!self.deferred_action_queue.isEmpty()) {
                    const action = self.deferred_action_queue.dequeue();
                    switch (action) {
                        .HandleResizeEvent => {
                            self.handleResizeEvent(action.HandleResizeEvent);
                        },
                        .TryUpdateOnce => {
                            try self.incomplete_data.container.append(action.TryUpdateOnce);
                            try self.tryUpdateOnce();
                        },
                    }
                }

                while (!self.loop_queue.isEmpty()) {
                    const code = self.loop_queue.dequeue();
                    switch (code) {
                        .Exit => {
                            self.finished = false;
                            if (self.input_error) |err| {
                                return err;
                            }
                            return self.returned_line;
                        },
                        .Retry => {
                            continue :start;
                        },
                    }
                }
            }
        }
    }

    fn controlThreadMain(self: *Self) void {
        var stdin = std.io.getStdIn();
        var buffer = [1]u8{0};
        while (true) {
            // FIXME: We shouldn't actually read here, but rather use select() or poll()
            //        However, Zig's interface to poll() reads from the fd (!), which is not what we want.
            //        And there's no select() interface at all, so we're stuck with this for now.
            const size = stdin.read(&buffer) catch |err| {
                self.input_error = err;
                self.loop_queue.enqueue(.Exit) catch {
                    break;
                };
                self.queue_condition.broadcast();
                break;
            };

            if (size == 1) {
                self.deferred_action_queue.enqueue(DeferredAction{ .TryUpdateOnce = buffer[0] }) catch {};
            }
            self.queue_condition.broadcast();
        }
    }

    fn initialize(self: *Self) !void {
        if (self.initialized) {
            return;
        }

        var termios = try getTermios();
        self.default_termios = termios;

        self.getTerminalSize();

        // FIXME: Windows
        termios.lflag &= ~std.os.linux.ECHO & ~std.os.linux.ICANON;

        try setTermios(termios);
        self.termios = termios;
        try self.setDefaultKeybinds();
        self.initialized = true;
    }

    pub fn interrupted(self: *Self) void {
        _ = self;
    }

    pub fn resized(self: *Self) void {
        _ = self;
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
        for (self.buffer.container.items[0..index]) |code_point| {
            var u8buf = [4]u8{ 0, 0, 0, 0 };
            const length = try std.unicode.utf8Encode(@intCast(u21, code_point), &u8buf);
            try u8buffer.appendSlice(u8buf[0..length]);
        }

        return u8buffer.toOwnedSlice();
    }

    pub fn setPrompt(self: *Self, prompt: []const u8) !void {
        if (self.cached_prompt_valid) {
            self.old_prompt_metrics = self.cached_prompt_metrics;
        }
        self.cached_prompt_valid = false;
        self.cached_prompt_metrics = Editor.actualRenderedStringMetrics(prompt);
        self.new_prompt.container.clearRetainingCapacity();
        try self.new_prompt.container.appendSlice(prompt);
    }

    pub fn actualRenderedStringMetrics(string: []const u8) StringMetrics {
        _ = string;
        return undefined;
    }

    pub fn actualRenderedUnicodeStringMetrics(string: []const u32) StringMetrics {
        _ = string;
        return undefined;
    }

    pub fn clearLine(self: *Self) void {
        _ = self;
    }

    pub fn insertString(self: *Self, string: []const u8) void {
        for (string) |code_point| {
            self.insertCodePoint(code_point);
        }
    }

    pub fn insertCodePoint(self: *Self, code_point: u32) void {
        var buf = [_]u8{ 0, 0, 0, 0 };
        const length = std.unicode.utf8Encode(@intCast(u21, code_point), &buf) catch {
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
        _ = self;
    }

    pub fn allowInput(self: *Self) void {
        _ = self;
    }

    fn setDefaultKeybinds(self: *Self) !void {
        // self.registerCharInputCallback(ctrl('N'), self.search_forwards);
        try self.registerCharInputCallback('\n', &Self.finish);
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

    fn vtApplyStyle(self: *Self, style: Style, output_stream: anytype) !void {
        _ = self;
        _ = style;
        _ = output_stream;
    }

    fn vtMoveAbsolute(self: *Self, row: usize, col: usize, output_stream: anytype) !void {
        _ = output_stream;
        _ = self;
        _ = row;
        _ = col;
    }

    fn vtClearToEndOfLine(self: *Self, output_stream: anytype) !void {
        _ = output_stream;
        _ = self;
    }

    fn tryUpdateOnce(self: *Self) !void {
        if (self.was_interrupted) {
            self.handleInterruptEvent();
        }

        try self.handleReadEvent();

        if (self.always_refresh) {
            self.refresh_needed = true;
        }

        try self.refreshDisplay();

        if (self.finished) {
            try self.reallyQuitEventLoop();
        }
    }

    fn handleInterruptEvent(self: *Self) void {
        _ = self;
    }

    fn handleReadEvent(self: *Self) !void {
        if (self.prohibit_input_processing) {
            self.have_unprocessed_read_event = true;
            return;
        }

        self.prohibit_input_processing = true;
        defer self.prohibit_input_processing = false;

        var keybuf = [16]u8{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
        var nread: usize = 0;

        var stdin = std.io.getStdIn();

        if (self.incomplete_data.container.items.len == 0) {
            nread = stdin.read(&keybuf) catch |err| {
                // Zig eats EINTR, so we'll have to delay resize handling until the next read.
                self.finished = true;
                self.input_error = err;
                return;
            };

            if (nread == 0) {
                self.input_error = error.Empty;
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
                    },
                },
                .CSIExpectParameter, .CSIExpectIntermediate, .CSIExpectFinal => {
                    if (self.input_state == .CSIExpectParameter) {
                        if (code_point >= 0x30 and code_point <= 0x3f) { // '0123456789:;<=>?'
                            try csi.parameter_bytes.append(@intCast(u8, code_point));
                            continue;
                        }
                        self.input_state = .CSIExpectIntermediate;
                    }
                    if (self.input_state == .CSIExpectIntermediate) {
                        if (code_point >= 0x20 and code_point <= 0x2f) { // ' !"#$%&\'()*+,-./'
                            try csi.intermediate_bytes.append(@intCast(u8, code_point));
                            continue;
                        }
                        self.input_state = .CSIExpectFinal;
                    }
                    if (self.input_state == .CSIExpectFinal) {
                        self.input_state = self.previous_free_state;
                        const is_in_paste = self.input_state == .Paste;
                        var it = std.mem.split(u8, csi.parameter_bytes.items, ";");
                        while (it.next()) |parameter| {
                            if (std.fmt.parseInt(u8, parameter, 10) catch null) |value| {
                                try csi_parameters.append(value);
                            } else {
                                try csi_parameters.append(0);
                            }
                        }
                        var param0: u32 = 0;
                        var param1: u32 = 0;
                        if (csi_parameters.items.len >= 1) {
                            param0 = csi_parameters.items[0];
                        }
                        if (csi_parameters.items.len >= 2) {
                            param1 = csi_parameters.items[1];
                        }

                        var modifiers: CSIMod = .None;
                        if (param1 != 0) {
                            modifiers = @intToEnum(CSIMod, @intCast(u8, param1 - 1));
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
                            std.log.debug("zigline: Invalid CSI: {x:02} ({c})", .{ code_point, @intCast(u8, code_point) });
                            continue;
                        }

                        csi_final = @intCast(u8, code_point);
                        csi_parameters.clearAndFree();
                        csi.parameter_bytes.clearAndFree();
                        csi.intermediate_bytes.clearAndFree();

                        if (csi_final == 'Z') {
                            // 'reverse tab'
                            reverse_tab = true;
                            break;
                        }

                        try self.cleanupSuggestions();

                        switch (csi_final) {
                            'A' => { // ^[[A: arrow up
                                // TODO: self.searchBackwards();
                                continue;
                            },
                            'B' => { // ^[[B: arrow down
                                // TODO: self.searchForwards();
                                continue;
                            },
                            'D' => { // ^[[D: arrow left
                                if (modifiers == .Alt or modifiers == .Ctrl) {
                                    // TODO: self.cursorLeftWord();
                                } else {
                                    // TODO: self.cursorLeftCharacter();
                                }
                                continue;
                            },
                            'C' => { // ^[[C: arrow right
                                if (modifiers == .Alt or modifiers == .Ctrl) {
                                    // TODO: self.cursorRightWord();
                                } else {
                                    // TODO: self.cursorRightCharacter();
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
                                    // TODO: self.eraseCharacterBackwards();
                                }
                                continue;
                            },
                            '~' => {
                                if (param1 == 3) { // ^[[3~: delete
                                    if (modifiers == .Ctrl) {
                                        // TODO: self.eraseAlnumWordForwards();
                                    } else {
                                        // TODO: self.eraseCharacterForwards();
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
                                        // TODO: on_paste
                                        if (self.paste_buffer.container.items.len > 0) {
                                            self.insertUtf32(self.paste_buffer.container.items);
                                        }
                                        continue;
                                    }
                                }
                                std.log.debug("zigline: Unhandled '~': {}", .{param1});
                                continue;
                            },
                            else => {
                                std.log.debug("zigline: Unhandled final: {x:02} ({c})", .{ code_point, @intCast(u8, code_point) });
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
                    // TODO: on_paste
                    self.insertCodePoint(code_point);
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
            // FIXME: Non-linux?
            if (code_point == self.termios.cc[std.os.linux.V.EOF] and self.buffer.container.items.len == 0) {
                self.finished = true;
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

        if (consumed_code_points == self.incomplete_data.container.items.len) {
            self.incomplete_data.container.clearAndFree();
        } else {
            for (consumed_code_points..self.incomplete_data.container.items.len) |_| {
                _ = self.incomplete_data.container.orderedRemove(consumed_code_points);
            }
        }

        if (self.incomplete_data.container.items.len != 0 and !self.finished) {
            try self.loop_queue.enqueue(.Retry);
            self.queue_condition.broadcast();
        }
    }

    fn handleResizeEvent(self: *Self, reset_origin: bool) void {
        _ = self;
        _ = reset_origin;
    }

    fn ensureFreeLinesFromOrigin(self: *Self, count: usize) void {
        _ = self;
        _ = count;
    }

    fn vtDSR(self: *Self) ![2]usize {
        _ = self;
        return undefined;
    }

    fn removeAtIndex(self: *Self, index: usize) void {
        _ = self;
        _ = index;
    }

    fn reset(self: *Self) void {
        _ = self;
    }

    fn search(self: *Self, phrase: []const u8, allow_empty: bool, from_beginning: bool) bool {
        _ = self;
        _ = phrase;
        _ = allow_empty;
        _ = from_beginning;
        return undefined;
    }

    fn endSearch(self: *Self) void {
        _ = self;
    }

    fn findApplicableStyle(self: *Self, index: usize) Style {
        _ = self;
        _ = index;
        return undefined;
    }

    fn refreshDisplay(self: *Self) !void {
        var buffered_output = std.io.bufferedWriter(std.io.getStdErr().writer());
        defer {
            self.shown_lines = self.currentPromptMetrics().lines_with_addition(self.cached_buffer_metrics, self.num_columns);
            _ = buffered_output.flush() catch {};
        }

        var has_cleaned_up = false;
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
        var current_num_lines = self.numLines();
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
            self.cached_buffer_metrics = actualRenderedUnicodeStringMetrics(self.buffer.container.items);
            self.drawn_end_of_line_offset = self.buffer.size();
            return;
        }

        // TODO: on_display_refresh

        if (self.cached_prompt_valid) {
            if (!self.refresh_needed and self.cursor == self.buffer.size()) {
                // Just write the characters out and continue,
                // no need to refresh anything else.
                _ = try buffered_output.write(self.pending_chars.container.items);
                self.pending_chars.container.clearAndFree();
                self.drawn_cursor = self.cursor;
                self.drawn_end_of_line_offset = self.buffer.size();
                self.cached_buffer_metrics = actualRenderedUnicodeStringMetrics(self.buffer.container.items);
                self.drawn_spans = self.current_spans;
                return;
            }
        }

        const empty_styles = AutoHashMap(u32, Style).init(self.allocator);

        // If there have been no changes to previous sections of the line (style or text)
        // just appen the new text with the appropriate styles.
        if (!self.always_refresh and self.cached_prompt_valid and self.chars_touched_in_the_middle == 0 and self.drawn_spans.containsUpToOffset(self.current_spans, self.drawn_cursor)) {
            const initial_style = self.findApplicableStyle(self.drawn_end_of_line_offset);
            try self.vtApplyStyle(initial_style, &buffered_output);

            for (self.drawn_end_of_line_offset..self.buffer.size()) |i| {
                try self.applyStyles(empty_styles, &buffered_output, i);
                try self.printCharacterAt(i, &buffered_output);
            }

            try self.vtApplyStyle(Style.resetStyle(), &buffered_output);
            self.pending_chars.container.clearAndFree();
            self.refresh_needed = false;
            self.cached_buffer_metrics = actualRenderedUnicodeStringMetrics(self.buffer.container.items);
            self.chars_touched_in_the_middle = 0;
            self.drawn_cursor = self.cursor;
            self.drawn_end_of_line_offset = self.buffer.size();

            // No need to reposition the cursor, it's already in the right place.
            return;
        }

        // Ouch, reflow entire line.
        if (!has_cleaned_up) {
            try self.cleanup();
        }

        try self.vtMoveAbsolute(self.origin_row, 0, &buffered_output);

        _ = try buffered_output.write(self.new_prompt.container.items);

        try self.vtClearToEndOfLine(&buffered_output);

        for (0..self.buffer.size()) |i| {
            try self.applyStyles(empty_styles, &buffered_output, i);
            try self.printCharacterAt(i, &buffered_output);
        }

        try self.vtApplyStyle(Style.resetStyle(), &buffered_output);

        self.pending_chars.container.clearAndFree();
        self.refresh_needed = false;
        self.cached_buffer_metrics = actualRenderedUnicodeStringMetrics(self.buffer.container.items);
        self.chars_touched_in_the_middle = 0;
        self.drawn_spans = self.current_spans;
        self.drawn_end_of_line_offset = self.buffer.size();
        self.cached_prompt_valid = true;

        try self.repositionCursor(&buffered_output, false);
    }

    fn applyStyles(self: *Self, empty_styles: AutoHashMap(u32, Style), output_stream: anytype, index: usize) !void {
        _ = index;
        _ = self;
        _ = empty_styles;
        _ = output_stream;
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
            try buffer.append(@intCast(u8, code_point + 64));
        } else {
            const c = @intCast(u21, code_point);
            const length = try std.unicode.utf8CodepointSequenceLength(c);
            try buffer.appendNTimes(0, length);
            _ = try std.unicode.utf8Encode(c, buffer.items[buffer.items.len - length .. buffer.items.len]);
        }

        _ = try output_stream.write(buffer.items);
    }

    fn cleanup(self: *Self) !void {
        _ = self;
    }

    fn cleanupSuggestions(self: *Self) !void {
        _ = self;
    }

    fn reallyQuitEventLoop(self: *Self) !void {
        self.finished = false;
        var stream = std.io.getStdErr().writer();
        try self.repositionCursor(&stream, true);
        _ = try stream.write("\n");

        var str = try self.getBufferedLine();
        self.buffer.container.clearAndFree();
        self.chars_touched_in_the_middle = 0;
        self.is_editing = false;
        self.returned_line = str;

        if (self.initialized) {
            self.restore();
        }

        try self.loop_queue.enqueue(.Exit);
        self.queue_condition.broadcast();
    }

    fn restore(self: *Self) void {
        _ = self;
    }

    fn currentPromptMetrics(self: *Self) StringMetrics {
        if (!self.cached_prompt_valid) {
            return self.old_prompt_metrics;
        }

        return self.cached_prompt_metrics;
    }

    fn numLines(self: *Self) usize {
        _ = self;
        return 0;
    }

    fn cursorLine(self: *Self) usize {
        _ = self;
        return 0;
    }

    fn offsetInLine(self: *Self) usize {
        _ = self;
        return 0;
    }

    fn setOrigin(self: *Self, quit_on_error: bool) bool {
        _ = self;
        _ = quit_on_error;
        return undefined;
    }

    fn recalculateOrigin(self: *Self) void {
        _ = self;
    }

    fn repositionCursor(self: *Self, output_stream: anytype, to_end: bool) !void {
        _ = self;
        _ = output_stream;
        _ = to_end;
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
        _ = self;
    }
};
