
const std = @import("std");

fn abs(x: i32) i32 {
    return if (x < 0) -x else x;
}

fn max(a: i32, b: i32) i32 {
    return if (a > b) a else b;
}

fn distance(x: i32, y: i32, z: i32) i32 {
    return @divFloor(abs(x) + abs(y) + abs(z), 2);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var x: i32 = 0;
    var y: i32 = 0;
    var z: i32 = 0;
    var max_distance: i32 = 0;

    var it = std.mem.split(u8, content, ",");
    while (it.next()) |dir| {
        switch (dir[0]) {
            'n' => {
                if (dir.len == 1) {
                    y += 1;
                    z -= 1;
                } else if (dir[1] == 'e') {
                    x += 1;
                    z -= 1;
                } else {
                    x -= 1;
                    y += 1;
                }
            },
            's' => {
                if (dir.len == 1) {
                    y -= 1;
                    z += 1;
                } else if (dir[1] == 'e') {
                    x += 1;
                    y -= 1;
                } else {
                    x -= 1;
                    z += 1;
                }
            },
            else => unreachable,
        }

        max_distance = max(max_distance, distance(x, y, z));
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{max_distance});
}
