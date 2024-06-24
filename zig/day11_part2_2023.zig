
const std = @import("std");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

const Coord = struct {
    x: usize,
    y: usize,
};

const Grid = struct {
    width: usize,
    height: usize,
    data: AutoHashMap(Coord, void),

    fn init(allocator: std.mem.Allocator) Grid {
        return .{
            .width = 0,
            .height = 0,
            .data = AutoHashMap(Coord, void).init(allocator),
        };
    }

    fn deinit(self: *Grid) void {
        self.data.deinit();
    }
};

fn buildGrid(allocator: std.mem.Allocator, input: []const []const u8) !Grid {
    var grid = Grid.init(allocator);
    grid.width = input[0].len;
    grid.height = input.len;

    for (input, 0..) |line, y| {
        for (line, 0..) |char, x| {
            if (char != '.') {
                try grid.data.put(.{ .x = x, .y = y }, {});
            }
        }
    }

    return grid;
}

fn getEmptyRows(grid: Grid, allocator: std.mem.Allocator) !ArrayList(usize) {
    var emptyRows = ArrayList(usize).init(allocator);
    outer: for (0..grid.height) |y| {
        for (0..grid.width) |x| {
            if (grid.data.contains(.{ .x = x, .y = y })) {
                continue :outer;
            }
        }
        try emptyRows.append(y);
    }
    return emptyRows;
}

fn getEmptyCols(grid: Grid, allocator: std.mem.Allocator) !ArrayList(usize) {
    var emptyCols = ArrayList(usize).init(allocator);
    outer: for (0..grid.width) |x| {
        for (0..grid.height) |y| {
            if (grid.data.contains(.{ .x = x, .y = y })) {
                continue :outer;
            }
        }
        try emptyCols.append(x);
    }
    return emptyCols;
}

fn calculateOffsets(emptyIndexes: []const usize, bound: usize, allocator: std.mem.Allocator) ![]usize {
    var offsets = try allocator.alloc(usize, bound);
    @memset(offsets, 0);
    for (emptyIndexes) |idx| {
        for (idx + 1..bound) |i| {
            offsets[i] += 1;
        }
    }
    return offsets;
}

fn expandGrid(grid: Grid, expansionFactor: usize, allocator: std.mem.Allocator) !Grid {
    var emptyCols = try getEmptyCols(grid, allocator);
    defer emptyCols.deinit();
    var emptyRows = try getEmptyRows(grid, allocator);
    defer emptyRows.deinit();
    const numLinesToAdd = expansionFactor - 1;

    var newGrid = Grid.init(allocator);
    newGrid.width = grid.width + emptyCols.items.len * numLinesToAdd;
    newGrid.height = grid.height + emptyRows.items.len * numLinesToAdd;

    var dXs = try calculateOffsets(emptyCols.items, grid.width, allocator);
    defer allocator.free(dXs);
    var dYs = try calculateOffsets(emptyRows.items, grid.height, allocator);
    defer allocator.free(dYs);

    var it = grid.data.keyIterator();
    while (it.next()) |coord| {
        const newCoord = Coord{
            .x = coord.x + dXs[coord.x] * numLinesToAdd,
            .y = coord.y + dYs[coord.y] * numLinesToAdd,
        };
        try newGrid.data.put(newCoord, {});
    }

    return newGrid;
}

fn calculateLength(c1: Coord, c2: Coord) usize {
    const dX = if (c2.x > c1.x) c2.x - c1.x else c1.x - c2.x;
    const dY = if (c2.y > c1.y) c2.y - c1.y else c1.y - c2.y;
    return dX + dY;
}

fn solve(input: []const []const u8, expansionFactor: usize, allocator: std.mem.Allocator) !usize {
    var grid = try buildGrid(allocator, input);
    defer grid.deinit();

    var expandedGrid = try expandGrid(grid, expansionFactor, allocator);
    defer expandedGrid.deinit();

    var res: usize = 0;
    var alreadySeen = AutoHashMap(Coord, void).init(allocator);
    defer alreadySeen.deinit();

    var it = expandedGrid.data.keyIterator();
    while (it.next()) |coord1| {
        var it2 = alreadySeen.keyIterator();
        while (it2.next()) |coord2| {
            res += calculateLength(coord1.*, coord2.*);
        }
        try alreadySeen.put(coord1.*, {});
    }

    return res;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var lines = ArrayList([]const u8).init(allocator);
    defer lines.deinit();

    var it = std.mem.split(u8, content, "\n");
    while (it.next()) |line| {
        if (line.len > 0) {
            try lines.append(line);
        }
    }

    const result = try solve(lines.items, 1000000, allocator);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}
