
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const input = try file.readToEndAlloc(allocator, 1024);
    defer allocator.free(input);

    var cups = try allocator.alloc(usize, input.len + 1);
    defer allocator.free(cups);

    var current_cup: usize = 0;
    for (input, 0..) |char, i| {
        const cup = char - '0';
        if (i == 0) current_cup = cup;
        if (i < input.len - 1) {
            cups[cup] = input[i + 1] - '0';
        }
    }
    cups[input[input.len - 1] - '0'] = input[0] - '0';

    var i: usize = 0;
    while (i < 100) : (i += 1) {
        const pickup1 = cups[current_cup];
        const pickup2 = cups[pickup1];
        const pickup3 = cups[pickup2];

        cups[current_cup] = cups[pickup3];

        var destination_cup: usize = if (current_cup > 1) current_cup - 1 else input.len;
        while (destination_cup == pickup1 or destination_cup == pickup2 or destination_cup == pickup3) {
            destination_cup = if (destination_cup > 1) destination_cup - 1 else input.len;
        }

        cups[pickup3] = cups[destination_cup];
        cups[destination_cup] = pickup1;

        current_cup = cups[current_cup];
    }

    var cup = cups[1];
    while (cup != 1) {
        try std.io.getStdOut().writer().print("{d}", .{cup});
        cup = cups[cup];
    }
    try std.io.getStdOut().writer().print("\n", .{});
}
