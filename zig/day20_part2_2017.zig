
const std = @import("std");

const Particle = struct {
    p: [3]i32,
    v: [3]i32,
    a: [3]i32,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var particles = std.ArrayList(Particle).init(allocator);
    defer particles.deinit();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();
    var buf: [1024]u8 = undefined;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var p: Particle = undefined;
        var it = std.mem.split(u8, line, ", ");
        inline for (0..3) |i| {
            const part = it.next().?;
            var coord_it = std.mem.split(u8, part[3 .. part.len - 1], ",");
            inline for (0..3) |j| {
                const num = try std.fmt.parseInt(i32, coord_it.next().?, 10);
                switch (i) {
                    0 => p.p[j] = num,
                    1 => p.v[j] = num,
                    2 => p.a[j] = num,
                    else => unreachable,
                }
            }
        }
        try particles.append(p);
    }

    var positions = std.AutoHashMap([3]i32, u32).init(allocator);
    defer positions.deinit();

    for (0..1000) |_| {
        positions.clearRetainingCapacity();

        for (particles.items) |*particle| {
            inline for (0..3) |j| {
                particle.v[j] += particle.a[j];
                particle.p[j] += particle.v[j];
            }
            const count = try positions.getOrPut(particle.p);
            if (!count.found_existing) {
                count.value_ptr.* = 0;
            }
            count.value_ptr.* += 1;
        }

        var i: usize = 0;
        while (i < particles.items.len) {
            if (positions.get(particles.items[i].p).? > 1) {
                _ = particles.swapRemove(i);
            } else {
                i += 1;
            }
        }
    }

    try std.io.getStdOut().writer().print("{d}\n", .{particles.items.len});
}
