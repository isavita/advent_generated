const std = @import("std");

fn calculateFuel(mass: i32) i32 {
    return @max(0, @divTrunc(mass, 3) - 2);
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = file.reader();
    var line_buffer: [1024]u8 = undefined;
    var total_fuel: i32 = 0;

    while (try reader.readUntilDelimiterOrEof(&line_buffer, '\n')) |line| {
        const mass = try std.fmt.parseInt(i32, line, 10);
        total_fuel += calculateFuel(mass);
    }

    std.debug.print("Total fuel requirement: {}\n", .{total_fuel});
}