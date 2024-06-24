
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

fn parseInput(allocator: Allocator, input: []const u8) !ArrayList(Component) {
    var components = ArrayList(Component).init(allocator);
    var lines = mem.split(u8, input, "\n");
    while (lines.next()) |line| {
        var ports = mem.split(u8, line, "/");
        const port1 = try std.fmt.parseInt(u32, ports.next() orelse return error.InvalidInput, 10);
        const port2 = try std.fmt.parseInt(u32, ports.next() orelse return error.InvalidInput, 10);
        try components.append(Component{ .port1 = port1, .port2 = port2 });
    }
    return components;
}

fn buildBridge(components: []const Component, used: []bool, lastPort: u32, strength: u32, length: u32, maxStrength: *u32, maxLength: *u32, maxLengthStrength: *u32) void {
    maxStrength.* = @max(maxStrength.*, strength);
    if (length > maxLength.*) {
        maxLength.* = length;
        maxLengthStrength.* = strength;
    } else if (length == maxLength.*) {
        maxLengthStrength.* = @max(maxLengthStrength.*, strength);
    }

    for (components, 0..) |component, i| {
        if (!used[i]) {
            if (component.port1 == lastPort or component.port2 == lastPort) {
                used[i] = true;
                const nextPort = if (component.port1 == lastPort) component.port2 else component.port1;
                buildBridge(components, used, nextPort, strength + component.port1 + component.port2, length + 1, maxStrength, maxLength, maxLengthStrength);
                used[i] = false;
            }
        }
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const input = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(input);

    var components = try parseInput(allocator, input);
    defer components.deinit();

    var used = try allocator.alloc(bool, components.items.len);
    defer allocator.free(used);
    @memset(used, false);

    var maxStrength: u32 = 0;
    var maxLength: u32 = 0;
    var maxLengthStrength: u32 = 0;

    buildBridge(components.items, used, 0, 0, 0, &maxStrength, &maxLength, &maxLengthStrength);

    const stdout = io.getStdOut().writer();
    try stdout.print("Part 1: {}\n", .{maxStrength});
    try stdout.print("Part 2: {}\n", .{maxLengthStrength});
}
