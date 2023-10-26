const Editor = @import("main.zig").Editor;
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var editor = Editor.init(gpa.allocator(), .{});

    var handler: struct {
        editor: *Editor,

        pub fn display_refresh(self: *@This()) void {
            self.editor.stripStyles();
            const buf = self.editor.getBuffer();
            for (buf, 0..) |c, i| {
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
    } = .{ .editor = &editor };
    editor.setHandler(&handler);

    const line: []const u8 = try editor.getLine("> ");
    defer gpa.allocator().free(line);
    defer editor.deinit();

    std.log.info("line: {s}\n", .{line});
}
