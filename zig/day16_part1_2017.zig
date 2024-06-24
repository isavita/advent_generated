
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
    var moves = std.mem.split(u8, content, ",");

    while (moves.next()) |move| {
        switch (move[0]) {
            's' => {
                const x = try std.fmt.parseInt(usize, move[1..], 10);
                std.mem.rotate(u8, &programs, programs.len - x);
            },
            'x' => {
                var positions = std.mem.split(u8, move[1..], "/");
                const a = try std.fmt.parseInt(usize, positions.next().?, 10);
                const b = try std.fmt.parseInt(usize, positions.next().?, 10);
                std.mem.swap(u8, &programs[a], &programs[b]);
            },
            'p' => {
                var positions = std.mem.split(u8, move[1..], "/");
                const a = positions.next().?[0];
                const b = positions.next().?[0];
                const ia = std.mem.indexOfScalar(u8, &programs, a).?;
                const ib = std.mem.indexOfScalar(u8, &programs, b).?;
                std.mem.swap(u8, &programs[ia], &programs[ib]);
            },
            else => unreachable,
        }
    }

    try std.io.getStdOut().writer().print("{s}\n", .{programs});
}
