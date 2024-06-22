const std = @import("std");

const Reindeer = struct {
    name: []const u8,
    speed: u32,
    fly_time: u32,
    rest_time: u32,
};

fn calculateDistance(reindeer: Reindeer, total_time: u32) u32 {
    const cycle_time = reindeer.fly_time + reindeer.rest_time;
    const full_cycles = total_time / cycle_time;
    const remaining_time = total_time % cycle_time;
    const fly_time = if (remaining_time > reindeer.fly_time) reindeer.fly_time else remaining_time;

    return (full_cycles * reindeer.fly_time + fly_time) * reindeer.speed;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var reindeer_list = std.ArrayList(Reindeer).init(allocator);
    defer reindeer_list.deinit();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var it = std.mem.split(u8, line, " ");
        const name = it.next().?;
        _ = it.next(); // skip "can"
        _ = it.next(); // skip "fly"
        const speed = try std.fmt.parseInt(u32, it.next().?, 10);
        _ = it.next(); // skip "km/s"
        _ = it.next(); // skip "for"
        const fly_time = try std.fmt.parseInt(u32, it.next().?, 10);
        _ = it.next(); // skip "seconds,"
        _ = it.next(); // skip "but"
        _ = it.next(); // skip "then"
        _ = it.next(); // skip "must"
        _ = it.next(); // skip "rest"
        _ = it.next(); // skip "for"
        const rest_time = try std.fmt.parseInt(u32, it.next().?, 10);

        try reindeer_list.append(Reindeer{
            .name = try allocator.dupe(u8, name),
            .speed = speed,
            .fly_time = fly_time,
            .rest_time = rest_time,
        });
    }

    const race_duration: u32 = 2503;
    var max_distance: u32 = 0;

    for (reindeer_list.items) |reindeer| {
        const distance = calculateDistance(reindeer, race_duration);
        if (distance > max_distance) {
            max_distance = distance;
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("The winning reindeer traveled {} km.\n", .{max_distance});

    for (reindeer_list.items) |reindeer| {
        allocator.free(reindeer.name);
    }
}
