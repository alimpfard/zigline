const Editor = @import("main.zig").Editor;
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var editor = Editor.init(gpa.allocator(), .{});
    const line = try editor.getLine("> ");
    defer editor.deinit();

    std.log.info("line: {s}\n", .{line});
}
