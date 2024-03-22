const std = @import("std");

pub fn main() !void {
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var reader = file.reader();
    var buffer: [1024]u8 = undefined;
    var x: i32 = 1;
    var cycle: i32 = 0;
    var sum: i32 = 0;

    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        if (std.mem.eql(u8, line, "noop")) {
            cycle += 1;
            if (cycle == 20 or @rem(cycle - 20, 40) == 0) {
                sum += cycle * x;
            }
        } else {
            const value = try std.fmt.parseInt(i32, line[5..], 10);
            cycle += 1;
            if (cycle == 20 or @rem(cycle - 20, 40) == 0) {
                sum += cycle * x;
            }
            cycle += 1;
            if (cycle == 20 or @rem(cycle - 20, 40) == 0) {
                sum += cycle * x;
            }
            x += value;
        }
    }

    std.debug.print("Sum of signal strengths: {}\n", .{sum});
}