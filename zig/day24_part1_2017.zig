
const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const Component = struct {
    port1: u32,
    port2: u32,
};

fn parseComponent(line: []const u8) !Component {
    var it = mem.split(u8, line, "/");
    const port1 = try std.fmt.parseInt(u32, it.next() orelse return error.InvalidInput, 10);
    const port2 = try std.fmt.parseInt(u32, it.next() orelse return error.InvalidInput, 10);
    return Component{ .port1 = port1, .port2 = port2 };
}

fn findStrongestBridge(components: []Component, start: u32, used: []bool, strength: u32) u32 {
    var maxStrength = strength;

    for (components, 0..) |comp, i| {
        if (used[i]) continue;

        if (comp.port1 == start or comp.port2 == start) {
            used[i] = true;
            const nextPort = if (comp.port1 == start) comp.port2 else comp.port1;
            const newStrength = strength + comp.port1 + comp.port2;
            const bridgeStrength = findStrongestBridge(components, nextPort, used, newStrength);
            maxStrength = @max(maxStrength, bridgeStrength);
            used[i] = false;
        }
    }

    return maxStrength;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var components = ArrayList(Component).init(allocator);
    defer components.deinit();

    var buf_reader = io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const component = try parseComponent(line);
        try components.append(component);
    }

    var used = try allocator.alloc(bool, components.items.len);
    defer allocator.free(used);
    @memset(used, false);

    const strongestBridge = findStrongestBridge(components.items, 0, used, 0);
    const stdout = io.getStdOut().writer();
    try stdout.print("Strength of the strongest bridge: {}\n", .{strongestBridge});
}
