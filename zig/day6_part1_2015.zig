const std = @import("std");

const Grid = struct {
    lights: [1000][1000]bool,

    pub fn init() Grid {
        return Grid{
            .lights = [_][1000]bool{[_]bool{false} ** 1000} ** 1000,
        };
    }

    pub fn turnOn(self: *Grid, x1: usize, y1: usize, x2: usize, y2: usize) void {
        var y = y1;
        while (y <= y2) : (y += 1) {
            var x = x1;
            while (x <= x2) : (x += 1) {
                self.lights[y][x] = true;
            }
        }
    }

    pub fn turnOff(self: *Grid, x1: usize, y1: usize, x2: usize, y2: usize) void {
        var y = y1;
        while (y <= y2) : (y += 1) {
            var x = x1;
            while (x <= x2) : (x += 1) {
                self.lights[y][x] = false;
            }
        }
    }

    pub fn toggle(self: *Grid, x1: usize, y1: usize, x2: usize, y2: usize) void {
        var y = y1;
        while (y <= y2) : (y += 1) {
            var x = x1;
            while (x <= x2) : (x += 1) {
                self.lights[y][x] = !self.lights[y][x];
            }
        }
    }

    pub fn countLit(self: *Grid) usize {
        var count: usize = 0;
        for (self.lights) |row| {
            for (row) |light| {
                if (light) count += 1;
            }
        }
        return count;
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

    var grid = Grid.init();

    var lines = std.mem.split(u8, content, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        try processInstruction(&grid, line);
    }

    const lit_count = grid.countLit();
    try std.io.getStdOut().writer().print("{d}\n", .{lit_count});
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
