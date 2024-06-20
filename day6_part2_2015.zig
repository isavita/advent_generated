const std = @import("std");

const Grid = struct {
    lights: [][]u32,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !Grid {
        var lights = try allocator.alloc([]u32, 1000);
        errdefer allocator.free(lights);

        for (lights) |*row| {
            row.* = try allocator.alloc(u32, 1000);
            @memset(row.*, 0);
        }

        return Grid{
            .lights = lights,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Grid) void {
        for (self.lights) |row| {
            self.allocator.free(row);
        }
        self.allocator.free(self.lights);
    }

    pub fn turnOn(self: *Grid, x1: usize, y1: usize, x2: usize, y2: usize) void {
        var y = y1;
        while (y <= y2) : (y += 1) {
            var x = x1;
            while (x <= x2) : (x += 1) {
                self.lights[y][x] += 1;
            }
        }
    }

    pub fn turnOff(self: *Grid, x1: usize, y1: usize, x2: usize, y2: usize) void {
        var y = y1;
        while (y <= y2) : (y += 1) {
            var x = x1;
            while (x <= x2) : (x += 1) {
                if (self.lights[y][x] > 0) {
                    self.lights[y][x] -= 1;
                }
            }
        }
    }

    pub fn toggle(self: *Grid, x1: usize, y1: usize, x2: usize, y2: usize) void {
        var y = y1;
        while (y <= y2) : (y += 1) {
            var x = x1;
            while (x <= x2) : (x += 1) {
                self.lights[y][x] += 2;
            }
        }
    }

    pub fn totalBrightness(self: *Grid) u64 {
        var total: u64 = 0;
        for (self.lights) |row| {
            for (row) |light| {
                total += light;
            }
        }
        return total;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input from file
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var grid = try Grid.init(allocator);
    defer grid.deinit();

    var lines = std.mem.split(u8, content, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        try processInstruction(&grid, line);
    }

    const total_brightness = grid.totalBrightness();
    try std.io.getStdOut().writer().print("{d}\n", .{total_brightness});
}

fn processInstruction(grid: *Grid, instruction: []const u8) !void {
    var words = std.mem.split(u8, instruction, " ");
    var action: enum { TurnOn, TurnOff, Toggle } = undefined;

    const first_word = words.next() orelse return error.InvalidInstruction;
    if (std.mem.eql(u8, first_word, "turn")) {
        const second_word = words.next() orelse return error.InvalidInstruction;
        if (std.mem.eql(u8, second_word, "on")) {
            action = .TurnOn;
        } else if (std.mem.eql(u8, second_word, "off")) {
            action = .TurnOff;
        } else {
            return error.InvalidInstruction;
        }
    } else if (std.mem.eql(u8, first_word, "toggle")) {
        action = .Toggle;
    } else {
        return error.InvalidInstruction;
    }

    const start = words.next() orelse return error.InvalidInstruction;
    _ = words.next() orelse return error.InvalidInstruction; // Skip "through"
    const end = words.next() orelse return error.InvalidInstruction;

    var start_coords = try parseCoordinates(start);
    var end_coords = try parseCoordinates(end);

    switch (action) {
        .TurnOn => grid.turnOn(start_coords[0], start_coords[1], end_coords[0], end_coords[1]),
        .TurnOff => grid.turnOff(start_coords[0], start_coords[1], end_coords[0], end_coords[1]),
        .Toggle => grid.toggle(start_coords[0], start_coords[1], end_coords[0], end_coords[1]),
    }
}

fn parseCoordinates(coord_str: []const u8) ![2]usize {
    var coords = std.mem.split(u8, coord_str, ",");
    const x = try std.fmt.parseInt(usize, coords.next() orelse return error.InvalidCoordinate, 10);
    const y = try std.fmt.parseInt(usize, coords.next() orelse return error.InvalidCoordinate, 10);
    return [2]usize{ x, y };
}
