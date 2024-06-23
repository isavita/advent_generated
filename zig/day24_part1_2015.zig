const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var weights = std.ArrayList(u64).init(allocator);
    defer weights.deinit();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const weight = try std.fmt.parseInt(u64, line, 10);
        try weights.append(weight);
    }

    const total_weight = sum(weights.items);
    const group_weight = total_weight / 3;

    const result = try findIdealConfiguration(allocator, weights.items, group_weight);
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Quantum entanglement: {}\n", .{result});
}

fn sum(slice: []const u64) u64 {
    var total: u64 = 0;
    for (slice) |num| {
        total += num;
    }
    return total;
}

fn findIdealConfiguration(allocator: Allocator, weights: []const u64, target_weight: u64) !u64 {
    var min_packages: usize = math.maxInt(usize);
    var min_entanglement: u64 = math.maxInt(u64);

    var combination = try allocator.alloc(bool, weights.len);
    defer allocator.free(combination);

    var i: usize = 1;
    while (i <= weights.len) : (i += 1) {
        try generateCombinations(allocator, weights, target_weight, combination, 0, i, 0, &min_packages, &min_entanglement);
        if (min_entanglement != math.maxInt(u64)) {
            break;
        }
    }

    return min_entanglement;
}

fn generateCombinations(
    allocator: Allocator,
    weights: []const u64,
    target_weight: u64,
    combination: []bool,
    start: usize,
    len: usize,
    index: usize,
    min_packages: *usize,
    min_entanglement: *u64,
) !void {
    if (index == len) {
        var group_weight: u64 = 0;
        var group_size: usize = 0;
        var entanglement: u64 = 1;

        for (combination, 0..) |selected, i| {
            if (selected) {
                group_weight += weights[i];
                group_size += 1;
                entanglement *= weights[i];
            }
        }

        if (group_weight == target_weight and group_size <= min_packages.*) {
            if (group_size < min_packages.*) {
                min_packages.* = group_size;
                min_entanglement.* = entanglement;
            } else if (entanglement < min_entanglement.*) {
                min_entanglement.* = entanglement;
            }
        }
        return;
    }

    var i = start;
    while (i < weights.len) : (i += 1) {
        combination[i] = true;
        try generateCombinations(allocator, weights, target_weight, combination, i + 1, len, index + 1, min_packages, min_entanglement);
        combination[i] = false;
    }
}
