
const std = @import("std");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var initial_state = ArrayList(u8).init(allocator);
    defer initial_state.deinit();

    var rules = AutoHashMap([5]u8, u8).init(allocator);
    defer rules.deinit();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (std.mem.indexOf(u8, line, "initial state:")) |_| {
            try initial_state.appendSlice(line[15..]);
        } else if (std.mem.indexOf(u8, line, "=>")) |idx| {
            var key: [5]u8 = undefined;
            @memcpy(&key, line[0..5]);
            try rules.put(key, line[idx + 3]);
        }
    }

    var state = AutoHashMap(i32, void).init(allocator);
    defer state.deinit();

    for (initial_state.items, 0..) |c, i| {
        if (c == '#') {
            try state.put(@intCast(i), {});
        }
    }

    var new_state = AutoHashMap(i32, void).init(allocator);
    defer new_state.deinit();

    var generation: usize = 0;
    while (generation < 20) : (generation += 1) {
        var min_pot: i32 = std.math.maxInt(i32);
        var max_pot: i32 = std.math.minInt(i32);
        var it = state.keyIterator();
        while (it.next()) |k| {
            min_pot = @min(min_pot, k.*);
            max_pot = @max(max_pot, k.*);
        }

        var i: i32 = min_pot - 2;
        while (i <= max_pot + 2) : (i += 1) {
            var pattern: [5]u8 = .{'.', '.', '.', '.', '.'};
            for (0..5) |j| {
                if (state.contains(i + @as(i32, @intCast(j)) - 2)) {
                    pattern[j] = '#';
                }
            }
            if (rules.get(pattern)) |v| {
                if (v == '#') {
                    try new_state.put(i, {});
                }
            }
        }
        std.mem.swap(AutoHashMap(i32, void), &state, &new_state);
        new_state.clearRetainingCapacity();
    }

    var sum: i32 = 0;
    var it = state.keyIterator();
    while (it.next()) |k| {
        sum += k.*;
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{sum});
}
