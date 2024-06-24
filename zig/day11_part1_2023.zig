
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

    fn init(allocator: std.mem.Allocator, width: usize, height: usize) Grid {
        return .{
            .width = width,
            .height = height,
            .data = AutoHashMap(Coord, void).init(allocator),
        };
    }

    fn deinit(self: *Grid) void {
        self.data.deinit();
    }
};

fn buildGrid(allocator: std.mem.Allocator, input: []const []const u8) !Grid {
    var grid = Grid.init(allocator, input[0].len, input.len);
    for (input, 0..) |line, y| {
        for (line, 0..) |char, x| {
            if (char != '.') {
                try grid.data.put(.{ .x = x, .y = y }, {});
            }
        }
    }
    return grid;
}

fn getEmptyRows(grid: Grid) !ArrayList(usize) {
    var emptyRows = ArrayList(usize).init(std.heap.page_allocator);
    var y: usize = 0;
    while (y < grid.height) : (y += 1) {
        var isEmpty = true;
        var x: usize = 0;
        while (x < grid.width) : (x += 1) {
            if (grid.data.contains(.{ .x = x, .y = y })) {
                isEmpty = false;
                break;
            }
        }
        if (isEmpty) {
            try emptyRows.append(y);
        }
    }
    return emptyRows;
}

fn getEmptyCols(grid: Grid) !ArrayList(usize) {
    var emptyCols = ArrayList(usize).init(std.heap.page_allocator);
    var x: usize = 0;
    while (x < grid.width) : (x += 1) {
        var isEmpty = true;
        var y: usize = 0;
        while (y < grid.height) : (y += 1) {
            if (grid.data.contains(.{ .x = x, .y = y })) {
                isEmpty = false;
                break;
            }
        }
        if (isEmpty) {
            try emptyCols.append(x);
        }
    }
    return emptyCols;
}

fn calculateLength(c1: Coord, c2: Coord) usize {
    const dx = if (c2.x > c1.x) c2.x - c1.x else c1.x - c2.x;
    const dy = if (c2.y > c1.y) c2.y - c1.y else c1.y - c2.y;
    return dx + dy;
}

fn solve(allocator: std.mem.Allocator, input: []const []const u8) !usize {
    var grid = try buildGrid(allocator, input);
    defer grid.deinit();

    var emptyCols = try getEmptyCols(grid);
    defer emptyCols.deinit();
    var emptyRows = try getEmptyRows(grid);
    defer emptyRows.deinit();

    var res: usize = 0;
    var it = grid.data.keyIterator();
    while (it.next()) |coord1| {
        var it2 = grid.data.keyIterator();
        while (it2.next()) |coord2| {
            if (coord1.x > coord2.x or (coord1.x == coord2.x and coord1.y > coord2.y)) continue;
            var length = calculateLength(coord1.*, coord2.*);
            for (emptyCols.items) |col| {
                if ((coord1.x < col and col < coord2.x) or (coord2.x < col and col < coord1.x)) {
                    length += 1;
                }
            }
            for (emptyRows.items) |row| {
                if ((coord1.y < row and row < coord2.y) or (coord2.y < row and row < coord1.y)) {
                    length += 1;
                }
            }
            res += length;
        }
    }

    return res;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var lines = ArrayList([]const u8).init(allocator);
    defer lines.deinit();

    var it = std.mem.split(u8, content, "\n");
    while (it.next()) |line| {
        try lines.append(line);
    }

    const result = try solve(allocator, lines.items);
    try std.io.getStdOut().writer().print("{}\n", .{result});
}
