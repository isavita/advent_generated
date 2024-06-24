
const std = @import("std");

const Neighbor = struct { dx: i32, dy: i32 };
const Neighbors8 = [_]Neighbor{
    .{ .dx = 0, .dy = 1 },  .{ .dx = 0, .dy = -1 },
    .{ .dx = 1, .dy = 0 },  .{ .dx = -1, .dy = 0 },
    .{ .dx = -1, .dy = -1 }, .{ .dx = -1, .dy = 1 },
    .{ .dx = 1, .dy = -1 },  .{ .dx = 1, .dy = 1 },
};

const Part = struct {
    xmin: usize,
    xmax: usize,
    y: usize,
    n: u32,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const input = try std.fs.cwd().readFileAlloc(allocator, "input.txt", 1024 * 1024);
    defer allocator.free(input);

    var grid = std.AutoHashMap(struct { x: usize, y: usize }, u8).init(allocator);
    defer grid.deinit();

    var parts = std.ArrayList(Part).init(allocator);
    defer parts.deinit();

    var y: usize = 0;
    var x: usize = 0;
    var curr: ?Part = null;

    for (input) |c| {
        if (c == '\n') {
            if (curr) |p| {
                try parts.append(p);
                curr = null;
            }
            y += 1;
            x = 0;
            continue;
        }

        try grid.put(.{ .x = x, .y = y }, c);

        if (c >= '0' and c <= '9') {
            if (curr) |*p| {
                p.xmax = x;
                p.n = p.n * 10 + (c - '0');
            } else {
                curr = Part{ .xmin = x, .xmax = x, .y = y, .n = c - '0' };
            }
        } else if (curr != null) {
            try parts.append(curr.?);
            curr = null;
        }

        x += 1;
    }

    if (curr) |p| {
        try parts.append(p);
    }

    var partsGrid = std.AutoHashMap(struct { x: usize, y: usize }, usize).init(allocator);
    defer partsGrid.deinit();

    for (parts.items, 0..) |p, i| {
        var px = p.xmin;
        while (px <= p.xmax) : (px += 1) {
            try partsGrid.put(.{ .x = px, .y = p.y }, i);
        }
    }

    var sum: u32 = 0;
    var it = grid.iterator();
    while (it.next()) |entry| {
        if (entry.value_ptr.* == '*') {
            var neighborParts = std.AutoHashMap(usize, void).init(allocator);
            defer neighborParts.deinit();

            for (Neighbors8) |n| {
                const nx = @as(i32, @intCast(entry.key_ptr.x)) + n.dx;
                const ny = @as(i32, @intCast(entry.key_ptr.y)) + n.dy;
                if (nx >= 0 and ny >= 0) {
                    if (partsGrid.get(.{ .x = @intCast(nx), .y = @intCast(ny) })) |i| {
                        try neighborParts.put(i, {});
                    }
                }
            }

            if (neighborParts.count() == 2) {
                var prod: u32 = 1;
                var nit = neighborParts.keyIterator();
                while (nit.next()) |i| {
                    prod *= parts.items[i.*].n;
                }
                sum += prod;
            }
        }
    }

    try std.io.getStdOut().writer().print("{}\n", .{sum});
}
