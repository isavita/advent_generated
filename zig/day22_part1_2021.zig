const std = @import("std");

const Instruction = struct {
    on: bool,
    x1: i32,
    x2: i32,
    y1: i32,
    y2: i32,
    z1: i32,
    z2: i32,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var instructions = std.ArrayList(Instruction).init(allocator);
    defer instructions.deinit();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var parts = std.mem.split(u8, line, " ");
        const on = std.mem.eql(u8, parts.next().?, "on");
        var ranges = std.mem.split(u8, parts.next().?, ",");
        var x_range = std.mem.split(u8, ranges.next().?[2..], "..");
        var y_range = std.mem.split(u8, ranges.next().?[2..], "..");
        var z_range = std.mem.split(u8, ranges.next().?[2..], "..");

        const x1 = try std.fmt.parseInt(i32, x_range.next().?, 10);
        const x2 = try std.fmt.parseInt(i32, x_range.next().?, 10);
        const y1 = try std.fmt.parseInt(i32, y_range.next().?, 10);
        const y2 = try std.fmt.parseInt(i32, y_range.next().?, 10);
        const z1 = try std.fmt.parseInt(i32, z_range.next().?, 10);
        const z2 = try std.fmt.parseInt(i32, z_range.next().?, 10);

        try instructions.append(.{
            .on = on,
            .x1 = x1,
            .x2 = x2,
            .y1 = y1,
            .y2 = y2,
            .z1 = z1,
            .z2 = z2,
        });
    }

    var grid = std.AutoHashMap([3]i32, bool).init(allocator);
    defer grid.deinit();

    for (instructions.items) |instr| {
        if (instr.x1 > 50 or instr.x2 < -50 or instr.y1 > 50 or instr.y2 < -50 or instr.z1 > 50 or instr.z2 < -50) {
            continue;
        }
        const x1 = @max(instr.x1, -50);
        const x2 = @min(instr.x2, 50);
        const y1 = @max(instr.y1, -50);
        const y2 = @min(instr.y2, 50);
        const z1 = @max(instr.z1, -50);
        const z2 = @min(instr.z2, 50);

        var x = x1;
        while (x <= x2) : (x += 1) {
            var y = y1;
            while (y <= y2) : (y += 1) {
                var z = z1;
                while (z <= z2) : (z += 1) {
                    if (instr.on) {
                        try grid.put([3]i32{ x, y, z }, true);
                    } else {
                        _ = grid.remove([3]i32{ x, y, z });
                    }
                }
            }
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{}\n", .{grid.count()});
}