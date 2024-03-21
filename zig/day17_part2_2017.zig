const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buffer: [100]u8 = undefined;
    const bytes_read = try file.readAll(&buffer);
    const steps = try std.fmt.parseInt(usize, buffer[0 .. bytes_read], 10);

    var current_pos: usize = 0;
    var value_after_zero: usize = 0;

    for (0 .. 50_000_000) |i| {
        current_pos = (current_pos + steps) % (i + 1);
        if (current_pos == 0) {
            value_after_zero = i + 1;
        }
        current_pos += 1;
    }

    std.debug.print("{}\n", .{value_after_zero});
}