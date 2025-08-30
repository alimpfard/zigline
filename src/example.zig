const Editor = @import("main.zig").Editor;
const std = @import("std");
const builtin = @import("builtin");

pub fn main() void {
    switch (builtin.os.tag) {
        .uefi => main_uefi(std.os.uefi.pool_allocator),
        else => {
            var gpa = std.heap.GeneralPurposeAllocator(.{}){};
            defer _ = gpa.deinit();
            main_generic(gpa.allocator()) catch |err| {
                std.debug.print("Error: {}\n", .{err});
            };
        },
    }
}

fn main_generic(allocator: std.mem.Allocator) Editor.Error!void {
    var editor = Editor.init(allocator, .{});
    defer editor.deinit();

    try editor.loadHistory("test.hist");
    defer editor.saveHistory("test.hist") catch unreachable;

    var handler: struct {
        editor: *Editor,

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
                            .rgb = [3]u8{ 255, 0, 0 },
                        },
                    }) catch unreachable;
                } else {
                    self.editor.stylize(.{
                        .begin = i,
                        .end = i + 1,
                    }, .{
                        .foreground = .{
                            .rgb = [3]u8{
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
    } = .{ .editor = &editor };
    editor.setHandler(&handler);

    while (true) {
        const line: []const u8 = editor.getLine("> ") catch |err| switch (err) {
            error.Eof => break,
            else => return err,
        };
        defer allocator.free(line);

        try editor.addToHistory(line);
        std.log.info("line ({} bytes): {s}\n", .{ line.len, line });

        if (std.mem.eql(u8, line, "quit")) {
            break;
        }
    }
}

const WriterContext = struct {
    console_out: *std.os.uefi.protocol.SimpleTextOutput,
    attribute: usize,
};

fn writeFn(context: *const anyopaque, bytes: []const u8) anyerror!usize {
    const writer_context: *const WriterContext = @alignCast(@ptrCast(context));
    const console_out = writer_context.console_out;
    _ = console_out.setAttribute(writer_context.attribute);
    for (bytes) |c| {
        if (c == '\n') {
            _ = console_out.outputString(@ptrCast(&[2]u16{ '\r', 0 }));
        }
        _ = console_out.outputString(@ptrCast(&[2]u16{ c, 0 }));
    }
    return bytes.len;
}

fn main_uefi(allocator: std.mem.Allocator) void {
    var editor = Editor.init(allocator, .{});
    defer editor.deinit();

    // kiesel: src/branch/main/src/uefi.zig:69
    const console_out = std.os.uefi.system_table.con_out.?;
    _ = console_out.reset(true);
    _ = console_out.clearScreen();

    const stdout: std.io.AnyWriter = .{
        .context = &WriterContext{
            .console_out = console_out,
            .attribute = 0x7, // white
        },
        .writeFn = writeFn,
    };

    while (true) {
        const line: []const u8 = editor.getLine("> ") catch |err| switch (err) {
            error.Eof => break,
            else => return,
        };
        defer allocator.free(line);

        editor.addToHistory(line) catch {
            stdout.print("Failed to add line to history\n", .{}) catch {};
        };
        stdout.print("line ({} bytes): {s}\n", .{ line.len, line }) catch {};

        if (std.mem.eql(u8, line, "quit")) {
            break;
        }
    }
}
