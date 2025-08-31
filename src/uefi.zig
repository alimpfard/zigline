const std = @import("std");

pub const Writer = struct {
    console_out: *std.os.uefi.protocol.SimpleTextOutput,
    attribute: std.os.uefi.protocol.SimpleTextOutput.Attribute,
    interface: std.Io.Writer,

    pub fn init(
        buffer: []u8,
        console_out: *std.os.uefi.protocol.SimpleTextOutput,
        attribute: std.os.uefi.protocol.SimpleTextOutput.Attribute,
    ) Writer {
        return .{
            .console_out = console_out,
            .attribute = attribute,
            .interface = .{
                .vtable = &.{
                    .drain = drain,
                },
                .buffer = buffer,
            },
        };
    }

    const WriteError =
        std.os.uefi.protocol.SimpleTextOutput.SetAttributeError ||
        std.os.uefi.protocol.SimpleTextOutput.OutputStringError;

    fn write(self: Writer, bytes: []const u8) WriteError!usize {
        try self.console_out.setAttribute(self.attribute);
        for (bytes) |c| {
            if (c == '\n') {
                _ = try self.console_out.outputString(&.{ '\r', 0 });
            }
            _ = try self.console_out.outputString(&.{ c, 0 });
        }
        return bytes.len;
    }

    fn drain(io_writer: *std.Io.Writer, data: []const []const u8, splat: usize) std.Io.Writer.Error!usize {
        const writer: *Writer = @alignCast(@fieldParentPtr("interface", io_writer));
        const buffered = io_writer.buffered();
        if (buffered.len != 0) {
            const n = writer.write(buffered) catch return error.WriteFailed;
            return io_writer.consume(n);
        }
        for (data[0 .. data.len - 1]) |buf| {
            if (buf.len == 0) continue;
            const n = writer.write(buf) catch return error.WriteFailed;
            return io_writer.consume(n);
        }
        const pattern = data[data.len - 1];
        if (pattern.len == 0 or splat == 0) return 0;
        const n = writer.write(pattern) catch return error.WriteFailed;
        return io_writer.consume(n);
    }
};

pub const Reader = struct {
    console_in: *std.os.uefi.protocol.SimpleTextInput,
    console_out: *std.os.uefi.protocol.SimpleTextOutput,
    attribute: std.os.uefi.protocol.SimpleTextOutput.Attribute,
    interface: std.Io.Reader,

    pub fn init(
        buffer: []u8,
        console_in: *std.os.uefi.protocol.SimpleTextInput,
        console_out: *std.os.uefi.protocol.SimpleTextOutput,
        attribute: std.os.uefi.protocol.SimpleTextOutput.Attribute,
    ) Reader {
        return .{
            .console_in = console_in,
            .console_out = console_out,
            .attribute = attribute,
            .interface = .{
                .vtable = &.{
                    .stream = stream,
                },
                .buffer = buffer,
                .seek = 0,
                .end = 0,
            },
        };
    }

    const ReadError =
        std.os.uefi.protocol.SimpleTextInput.ReadKeyStrokeError ||
        std.os.uefi.protocol.SimpleTextOutput.EnableCursorError ||
        std.os.uefi.protocol.SimpleTextOutput.OutputStringError ||
        std.os.uefi.protocol.SimpleTextOutput.SetAttributeError ||
        std.os.uefi.tables.BootServices.WaitForEventError;

    fn read(self: Reader, bytes: []u8) ReadError!usize {
        try self.console_out.setAttribute(self.attribute);
        const boot_services = std.os.uefi.system_table.boot_services.?;
        var n: usize = 0;
        while (n < bytes.len) {
            _ = try boot_services.waitForEvent(&.{self.console_in.wait_for_key});
            const key = try self.console_in.readKeyStroke();
            if (key.scan_code != 0) continue;
            switch (key.unicode_char) {
                // Backspace
                0x08 => |c| {
                    // Only echo back to the output console if we have input buffered to not erase the prompt
                    if (n == 0) continue;
                    n -= 1;
                    _ = try self.console_out.outputString(&.{ c, 0 });
                },
                '\r' => {
                    bytes[n] = '\n';
                    n += 1;
                    _ = try self.console_out.outputString(&.{ '\r', '\n', 0 });
                    break;
                },
                else => |c| {
                    n += std.unicode.utf8Encode(c, bytes[n..]) catch 0;
                    _ = try self.console_out.outputString(&.{ c, 0 });
                },
            }
        }
        return n;
    }

    fn stream(io_reader: *std.Io.Reader, writer: *std.Io.Writer, limit: std.Io.Limit) std.Io.Reader.StreamError!usize {
        const reader: *Reader = @alignCast(@fieldParentPtr("interface", io_reader));
        const dest = limit.slice(try writer.writableSliceGreedy(1));
        const n = reader.read(dest) catch return error.ReadFailed;
        writer.advance(n);
        return n;
    }
};
