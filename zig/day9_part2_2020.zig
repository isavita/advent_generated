const std = @import("std");

const PREAMBLE_LENGTH = 25;

fn findInvalidNumber(numbers: []u64) u64 {
    var i: usize = PREAMBLE_LENGTH;
    while (i < numbers.len) : (i += 1) {
        if (!isValidNumber(numbers, i)) {
            return numbers[i];
        }
    }
    return 0;
}

fn isValidNumber(numbers: []u64, index: usize) bool {
    const target = numbers[index];
    var i: usize = index - PREAMBLE_LENGTH;
    while (i < index) : (i += 1) {
        var j: usize = i + 1;
        while (j < index) : (j += 1) {
            if (numbers[i] + numbers[j] == target) {
                return true;
            }
        }
    }
    return false;
}

fn findEncryptionWeakness(numbers: []u64, invalid_number: u64) u64 {
    var start: usize = 0;
    var end: usize = 1;
    var sum: u64 = numbers[start] + numbers[end];

    while (end < numbers.len) {
        if (sum == invalid_number) {
            return findMinMaxSum(numbers[start..end + 1]);
        } else if (sum < invalid_number) {
            end += 1;
            sum += numbers[end];
        } else {
            sum -= numbers[start];
            start += 1;
        }
    }
    return 0;
}

fn findMinMaxSum(range: []u64) u64 {
    var min: u64 = range[0];
    var max: u64 = range[0];
    for (range) |num| {
        if (num < min) min = num;
        if (num > max) max = num;
    }
    return min + max;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var numbers = std.ArrayList(u64).init(allocator);
    defer numbers.deinit();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const num = try std.fmt.parseInt(u64, line, 10);
        try numbers.append(num);
    }

    const invalid_number = findInvalidNumber(numbers.items);
    const weakness = findEncryptionWeakness(numbers.items, invalid_number);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Encryption Weakness: {}\n", .{weakness});
}