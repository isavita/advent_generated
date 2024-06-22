const std = @import("std");

const GRID_SIZE = 100;
const STEPS = 100;

const Grid = [GRID_SIZE][GRID_SIZE]bool;

fn countNeighbors(grid: Grid, x: usize, y: usize) u8 {
    var count: u8 = 0;
    const start_x = if (x == 0) 0 else x - 1;
    const end_x = if (x == GRID_SIZE - 1) x else x + 1;
    const start_y = if (y == 0) 0 else y - 1;
    const end_y = if (y == GRID_SIZE - 1) y else y + 1;

    var i = start_x;
    while (i <= end_x) : (i += 1) {
        var j = start_y;
        while (j <= end_y) : (j += 1) {
            if (i == x and j == y) continue;
            if (grid[i][j]) count += 1;
        }
    }
    return count;
}

fn simulateStep(grid: *Grid) void {
    var new_grid: Grid = undefined;

    for (grid, 0..) |row, x| {
        for (row, 0..) |light, y| {
            const neighbors = countNeighbors(grid.*, x, y);
            new_grid[x][y] = if (light) 
                (neighbors == 2 or neighbors == 3)
            else
                (neighbors == 3);
        }
    }

    grid.* = new_grid;
}

fn countLightsOn(grid: Grid) usize {
    var count: usize = 0;
    for (grid) |row| {
        for (row) |light| {
            if (light) count += 1;
        }
    }
    return count;
}

pub fn main() !void {
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var grid: Grid = undefined;

    var y: usize = 0;
    var buf: [GRID_SIZE + 1]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| : (y += 1) {
        for (line, 0..) |char, x| {
            grid[y][x] = char == '#';
        }
    }

    var step: usize = 0;
    while (step < STEPS) : (step += 1) {
        simulateStep(&grid);
    }

    const lights_on = countLightsOn(grid);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Number of lights on after {} steps: {}\n", .{ STEPS, lights_on });
}
