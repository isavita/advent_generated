const std = @import("std");

fn calculateFuel(mass: i32) i32 {
    const fuelRequired = @divTrunc(mass, 3) - 2;
    return @max(fuelRequired, 0);
}

fn calculateTotalFuel(mass: i32) i32 {
    var totalFuel: i32 = 0;
    var currentFuel = calculateFuel(mass);
    while (currentFuel > 0) {
        totalFuel += currentFuel;
        currentFuel = calculateFuel(currentFuel);
    }
    return totalFuel;
}

pub fn main() !void {
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var totalFuel: i32 = 0;
    var buf: [1024]u8 = undefined;
    while (try file.reader().readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const mass = try std.fmt.parseInt(i32, line, 10);
        totalFuel += calculateTotalFuel(mass);
    }

    std.debug.print("{}\n", .{totalFuel});
}