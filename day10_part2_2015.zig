const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input from file
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024);
    defer allocator.free(content);

    var sequence = try std.ArrayList(u8).initCapacity(allocator, content.len);
    defer sequence.deinit();

    try sequence.appendSlice(std.mem.trim(u8, content, &std.ascii.whitespace));

    var i: usize = 0;
    while (i < 50) : (i += 1) {
        try lookAndSay(&sequence, allocator);
        // Print progress every 10 iterations
        if ((i + 1) % 10 == 0) {
            std.debug.print("Completed iteration {d}, current length: {d}\n", .{ i + 1, sequence.items.len });
        }
    }

    const result = sequence.items.len;
    try std.io.getStdOut().writer().print("{d}\n", .{result});
}

fn lookAndSay(sequence: *std.ArrayList(u8), allocator: std.mem.Allocator) !void {
    var new_sequence = try std.ArrayList(u8).initCapacity(allocator, sequence.items.len * 2);
    defer new_sequence.deinit();

    var current_digit: u8 = sequence.items[0];
    var count: usize = 1;

    for (sequence.items[1..]) |digit| {
        if (digit == current_digit) {
            count += 1;
        } else {
            try appendDigits(&new_sequence, count);
            try new_sequence.append(current_digit);
            current_digit = digit;
            count = 1;
        }
    }

    try appendDigits(&new_sequence, count);
    try new_sequence.append(current_digit);

    sequence.clearAndFree();
    try sequence.appendSlice(new_sequence.items);
}

fn appendDigits(sequence: *std.ArrayList(u8), number: usize) !void {
    var n = number;
    var digits = std.ArrayList(u8).init(sequence.allocator);
    defer digits.deinit();

    while (n > 0) : (n /= 10) {
        try digits.append(@intCast(n % 10 + '0'));
    }

    var i = digits.items.len;
    while (i > 0) {
        i -= 1;
        try sequence.append(digits.items[i]);
    }
}
