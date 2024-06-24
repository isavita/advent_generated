
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var programs = [_]u8{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p'};
    const initial = programs;
    var cycleLen: usize = 0;

    outer: for (0..1000000000) |i| {
        var it = std.mem.split(u8, content, ",");
        while (it.next()) |move| {
            switch (move[0]) {
                's' => spin(&programs, try std.fmt.parseInt(usize, move[1..], 10)),
                'x' => {
                    const slash = std.mem.indexOf(u8, move, "/").?;
                    const a = try std.fmt.parseInt(usize, move[1..slash], 10);
                    const b = try std.fmt.parseInt(usize, move[slash + 1 ..], 10);
                    std.mem.swap(u8, &programs[a], &programs[b]);
                },
                'p' => {
                    const a = move[1];
                    const b = move[3];
                    const ia = std.mem.indexOf(u8, &programs, &[_]u8{a}).?;
                    const ib = std.mem.indexOf(u8, &programs, &[_]u8{b}).?;
                    std.mem.swap(u8, &programs[ia], &programs[ib]);
                },
                else => unreachable,
            }
        }
        if (std.mem.eql(u8, &programs, &initial)) {
            cycleLen = i + 1;
            break :outer;
        }
    }

    programs = initial;
    const remainingDances = 1000000000 % cycleLen;

    for (0..remainingDances) |_| {
        var it = std.mem.split(u8, content, ",");
        while (it.next()) |move| {
            switch (move[0]) {
                's' => spin(&programs, try std.fmt.parseInt(usize, move[1..], 10)),
                'x' => {
                    const slash = std.mem.indexOf(u8, move, "/").?;
                    const a = try std.fmt.parseInt(usize, move[1..slash], 10);
                    const b = try std.fmt.parseInt(usize, move[slash + 1 ..], 10);
                    std.mem.swap(u8, &programs[a], &programs[b]);
                },
                'p' => {
                    const a = move[1];
                    const b = move[3];
                    const ia = std.mem.indexOf(u8, &programs, &[_]u8{a}).?;
                    const ib = std.mem.indexOf(u8, &programs, &[_]u8{b}).?;
                    std.mem.swap(u8, &programs[ia], &programs[ib]);
                },
                else => unreachable,
            }
        }
    }

    try std.io.getStdOut().writer().print("{s}\n", .{programs});
}

fn spin(programs: []u8, x: usize) void {
    const n = programs.len;
    var temp: [16]u8 = undefined;
    @memcpy(&temp, programs);
    for (0..n) |i| {
        programs[(i + x) % n] = temp[i];
    }
}
