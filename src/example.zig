const Editor = @import("main.zig").Editor;
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    var editor = Editor.init(gpa.allocator(), .{});
    const line: []const u8 = try editor.getLine("> ");
    defer gpa.allocator().free(line);
    defer editor.deinit();

    std.log.info("line: {s}\n", .{line});
}
