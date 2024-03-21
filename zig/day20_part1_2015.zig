const std = @import("std");

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf: [10]u8 = undefined;
    const n = try file.reader().readAll(&buf);
    const presents = try std.fmt.parseInt(usize, buf[0 .. n], 10);

    var house: usize = 1;
    while (true) {
        var total: usize = 0;
        var i: usize = 1;
        while (i * i <= house) {
            if (house % i == 0) {
                total += i * 10;
                if (i * i != house) {
                    total += (house / i) * 10;
                }
            }
            i += 1;
        }
        if (total >= presents) {
            std.debug.print("{}\n", .{house});
            return;
        }
        house += 1;
    }
}