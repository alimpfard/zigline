const Editor = @import("main.zig").Editor;
const uefi = @import("uefi.zig");
const std = @import("std");
const builtin = @import("builtin");

pub const std_options: std.Options = switch (builtin.os.tag) {
    .uefi => .{
        // std.log does not work on UEFI (yet)
        .logFn = struct {
            fn logFn(
                comptime _: std.log.Level,
                comptime _: @TypeOf(.enum_literal),
                comptime _: []const u8,
                _: anytype,
            ) void {}
        }.logFn,
    },
    else => .{},
};

pub fn main() void {
    switch (builtin.os.tag) {
        .uefi => main_uefi(std.os.uefi.pool_allocator) catch {},
        else => {
            var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
            defer _ = debug_allocator.deinit();
            main_generic(debug_allocator.allocator()) catch |err| {
                std.debug.print("Error: {t}\n", .{err});
            };
        },
    }
}

fn main_generic(allocator: std.mem.Allocator) !void {
    var editor = Editor.init(allocator, .{});
    defer editor.deinit();

    try editor.loadHistory("test.hist");
    defer editor.saveHistory("test.hist") catch unreachable;

    var handler: struct {
        editor: *Editor,
        completion_storage: [4]Editor.CompletionSuggestion = @splat(.{ .text = "" }),

        pub fn display_refresh(self: *@This()) void {
            self.editor.stripStyles();
            const buf = self.editor.getBuffer();
            for (buf, 0..) |c, i| {
                if (c > 0x7F) {
                    self.editor.stylize(.{
                        .begin = i,
                        .end = i + 1,
                    }, .{
                        .foreground = .{
                            .rgb = .{ 255, 0, 0 },
                        },
                    }) catch unreachable;
                } else {
                    self.editor.stylize(.{
                        .begin = i,
                        .end = i + 1,
                    }, .{
                        .foreground = .{
                            .rgb = .{
                                @intCast(c % 26 * 10),
                                @intCast(c / 26 * 10),
                                @intCast(c % 10 * 10 + 150),
                            },
                        },
                    }) catch unreachable;
                }
            }
        }

        pub fn paste(self: *@This(), text: []const u32) void {
            self.editor.insertCodePoint('[');
            self.editor.insertUtf32(text);
            self.editor.insertCodePoint(']');
        }

        pub fn tab_complete(self: *@This()) ![]const Editor.CompletionSuggestion {
            var count: usize = 1;
            self.completion_storage[0] = if (self.editor.cursor == 0)
                .{
                    .text = "t",
                    .start_index = 0,
                }
            else b: {
                const next = switch (self.editor.getBuffer()[self.editor.cursor - 1]) {
                    't' => "e",
                    'e' => "s",
                    's' => "t",
                    else => return &.{},
                };
                break :b .{
                    .text = next,
                    .start_index = self.editor.cursor,
                    .allow_commit_without_listing = false,
                };
            };
            switch (self.completion_storage[count - 1].text[0]) {
                's' => {
                    self.completion_storage[count] = .{
                        .text = "st",
                        .start_index = self.editor.cursor,
                    };
                    count += 1;
                },
                't' => {
                    self.completion_storage[count] = .{
                        .text = "test2",
                        .start_index = self.editor.cursor,
                    };
                    count += 1;
                },
                else => {},
            }
            if (count == 2) {
                self.completion_storage[count] = .{
                    .text = "toast",
                    .start_index = self.editor.cursor,
                };
                count += 1;
            }

            return self.completion_storage[0..count];
        }
    } = .{ .editor = &editor };
    editor.setHandler(&handler);

    while (true) {
        const line: []const u8 = editor.getLine("> ") catch |err| switch (err) {
            error.Eof => break,
            else => return err,
        };
        defer allocator.free(line);

        try editor.addToHistory(line);
        std.log.info("line ({d} bytes): {s}\n", .{ line.len, line });

        if (std.mem.eql(u8, line, "quit")) {
            break;
        }
    }
}

fn main_uefi(allocator: std.mem.Allocator) !void {
    var editor = Editor.init(allocator, .{});
    defer editor.deinit();

    // kiesel: src/branch/main/src/uefi.zig
    const console_out = std.os.uefi.system_table.con_out.?;
    try console_out.reset(true);
    try console_out.clearScreen();
    try console_out.enableCursor(true);

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer: uefi.Writer = .init(&stdout_buffer, console_out, .{ .foreground = .white });
    const stdout = &stdout_writer.interface;

    while (true) {
        const line: []const u8 = editor.getLine("> ") catch |err| switch (err) {
            error.Eof => break,
            else => return err,
        };
        defer allocator.free(line);

        try editor.addToHistory(line);
        try stdout.print("line ({d} bytes): {s}\n", .{ line.len, line });
        try stdout.flush();

        if (std.mem.eql(u8, line, "quit")) {
            break;
        }
    }
}
