
const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;

const FACTOR_A: u64 = 16807;
const FACTOR_B: u64 = 48271;
const DIVISOR: u64 = 2147483647;

fn generateNext(prev: u64, factor: u64) u64 {
    return (prev * factor) % DIVISOR;
}

fn generateNextMultiple(prev: u64, factor: u64, multiple: u64) u64 {
    var next = prev;
    while (true) {
        next = generateNext(next, factor);
        if (next % multiple == 0) return next;
    }
}

fn countMatches(start_a: u64, start_b: u64, pairs: u64, use_multiples: bool) u64 {
    var a = start_a;
    var b = start_b;
    var count: u64 = 0;

    var i: u64 = 0;
    while (i < pairs) : (i += 1) {
        if (use_multiples) {
            a = generateNextMultiple(a, FACTOR_A, 4);
            b = generateNextMultiple(b, FACTOR_B, 8);
        } else {
            a = generateNext(a, FACTOR_A);
            b = generateNext(b, FACTOR_B);
        }

        if ((a & 0xFFFF) == (b & 0xFFFF)) {
            count += 1;
        }
    }

    return count;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var lines = mem.split(u8, content, "\n");
    const start_a = try std.fmt.parseInt(u64, mem.trimLeft(u8, lines.next().?, "Generator A starts with "), 10);
    const start_b = try std.fmt.parseInt(u64, mem.trimLeft(u8, lines.next().?, "Generator B starts with "), 10);

    const part1 = countMatches(start_a, start_b, 40_000_000, false);
    const part2 = countMatches(start_a, start_b, 5_000_000, true);

    const stdout = io.getStdOut().writer();
    try stdout.print("Part 1: {}\n", .{part1});
    try stdout.print("Part 2: {}\n", .{part2});
}
