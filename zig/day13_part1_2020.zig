const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var lines = std.mem.split(u8, content, "\n");
    const earliest_timestamp = try std.fmt.parseInt(u64, lines.next().?, 10);

    var bus_ids = std.mem.split(u8, lines.next().?, ",");
    var min_wait_time: u64 = std.math.maxInt(u64);
    var best_bus_id: u64 = 0;

    while (bus_ids.next()) |bus_id| {
        if (std.mem.eql(u8, bus_id, "x")) continue;

        const id = try std.fmt.parseInt(u64, bus_id, 10);
        const wait_time = id - (earliest_timestamp % id);
        if (wait_time < min_wait_time) {
            min_wait_time = wait_time;
            best_bus_id = id;
        }
    }

    const result = best_bus_id * min_wait_time;
    try std.io.getStdOut().writer().print("{d}\n", .{result});
}