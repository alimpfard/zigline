const std = @import("std");
const Allocator = std.mem.Allocator;

fn DeinitingAutoHashMap(comptime K: type, comptime V: type, comptime deinit_key: anytype, comptime deinit_value: anytype) type {
    return struct {
        container: std.AutoHashMap(K, V),

        const Self = @This();
        pub const Entry = std.AutoHashMap(K, V).Entry;

        pub fn deinitEntries(self: *Self) void {
            var iterator = self.container.iterator();
            while (iterator.next()) |entry| {
                deinit_value(entry.value_ptr);
                deinit_key(entry.key_ptr);
            }
        }

        pub fn init(allocator: Allocator) Self {
            return .{
                .container = std.AutoHashMap(K, V).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.deinitEntries();
            self.container.deinit();
        }
    };
}

fn DeinitingArrayList(comptime T: type, comptime deinit_fn: anytype) type {
    return struct {
        container: std.ArrayList(T),

        const Self = @This();

        pub fn deinitEntries(self: *Self) void {
            for (self.container.items) |*item| {
                deinit_fn(item);
            }
        }

        pub fn init(allocator: Allocator) Self {
            return Self{
                .container = std.ArrayList(T).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.deinitEntries();
            self.container.deinit();
        }

        // ffs zig
        pub fn size(self: Self) usize {
            return self.container.items.len;
        }
    };
}

fn deinitFnFor(comptime T: type) fn (ptr: *T) void {
    const t = @typeInfo(T);
    switch (t) {
        .Struct => {
            for (t.Struct.decls) |decl| {
                if (std.mem.eql(u8, decl.name, "deinit")) {
                    return T.deinit;
                }
            }
        },
        else => {},
    }

    return (struct {
        pub fn deinit(_: *T) void {}
    }).deinit;
}

pub fn AutoHashMap(comptime K: type, comptime V: type) type {
    return DeinitingAutoHashMap(K, V, deinitFnFor(K), deinitFnFor(V));
}

pub fn ArrayList(comptime T: type) type {
    return DeinitingArrayList(T, deinitFnFor(T));
}

pub fn Queue(comptime T: type) type {
    return struct {
        container: ArrayList(T),

        const Self = @This();
        pub fn init(allocator: Allocator) Self {
            return Self{
                .container = ArrayList(T).init(allocator),
            };
        }

        pub fn deinit(self: *Self) void {
            self.container.deinit();
        }

        pub fn enqueue(self: *Self, item: T) !void {
            try self.container.container.append(item);
        }

        pub fn dequeue(self: *Self) T {
            return self.container.container.orderedRemove(0);
        }

        pub fn isEmpty(self: Self) bool {
            return self.container.container.items.len == 0;
        }
    };
}

fn Checkpoint(comptime T: type) type {
    return struct {
        v: T,
        p: *T,
        pub fn restore(self: @This()) void {
            self.p.* = self.v;
        }
    };
}

fn checkpointImpl(comptime T: type, value: *T) Checkpoint(T) {
    return .{
        .v = value.*,
        .p = value,
    };
}

pub fn checkpoint(value: anytype) Checkpoint(@TypeOf(value.*)) {
    return checkpointImpl(@TypeOf(value.*), value);
}

pub const StringBuilder = struct {
    buffer: ArrayList(u8),

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return .{
            .buffer = ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.buffer.deinit();
    }

    pub fn appendCodePoint(self: *Self, c: u32) !void {
        var buf: [4]u8 = [_]u8{0} ** 4;
        const length = try std.unicode.utf8Encode(@intCast(c), &buf);
        try self.buffer.container.appendSlice(buf[0..length]);
    }

    pub fn appendSlice(self: *Self, slice: []const u8) !void {
        try self.buffer.container.appendSlice(slice);
    }

    pub fn appendUtf32Slice(self: *Self, slice: []const u32) !void {
        for (slice) |c| {
            try self.appendCodePoint(c);
        }
    }

    pub fn toOwnedSlice(self: *Self) ![]u8 {
        return self.buffer.container.toOwnedSlice();
    }

    pub fn toSlice(self: *Self) []u8 {
        return self.buffer.container.items;
    }
};
