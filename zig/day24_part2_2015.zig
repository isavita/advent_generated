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
    const group_weight = total_weight / 4;

    const result = try findIdealConfiguration(allocator, weights.items, group_weight);
    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}\n", .{result});
}

fn sum(slice: []const u64) u64 {
    var total: u64 = 0;
    for (slice) |value| {
        total += value;
    }
    return total;
}

fn findIdealConfiguration(allocator: Allocator, weights: []const u64, target_weight: u64) !u64 {
    var min_size: usize = math.maxInt(usize);
    var min_qe: u64 = math.maxInt(u64);

    var combination = try allocator.alloc(bool, weights.len);
    defer allocator.free(combination);
    @memset(combination, false);

    while (nextCombination(combination)) {
        const group_sum = sumSelected(weights, combination);
        if (group_sum == target_weight) {
            const size = countSelected(combination);
            if (size < min_size or (size == min_size and calculateQE(weights, combination) < min_qe)) {
                min_size = size;
                min_qe = calculateQE(weights, combination);
            }
        }
    }

    return min_qe;
}

fn nextCombination(combination: []bool) bool {
    var i: usize = 0;
    while (i < combination.len) : (i += 1) {
        if (!combination[i]) {
            combination[i] = true;
            return true;
        }
        combination[i] = false;
    }
    return false;
}

fn sumSelected(weights: []const u64, combination: []const bool) u64 {
    var summ: u64 = 0;
    for (weights, combination) |weight, selected| {
        if (selected) {
            summ += weight;
        }
    }
    return summ;
}

fn countSelected(combination: []const bool) usize {
    var count: usize = 0;
    for (combination) |selected| {
        if (selected) {
            count += 1;
        }
    }
    return count;
}

fn calculateQE(weights: []const u64, combination: []const bool) u64 {
    var qe: u64 = 1;
    for (weights, combination) |weight, selected| {
        if (selected) {
            qe *= weight;
        }
    }
    return qe;
}
