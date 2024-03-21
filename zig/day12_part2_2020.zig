const std = @import("std");

const Ship = struct {
    x: i32,
    y: i32,
    waypoint_x: i32,
    waypoint_y: i32,

    fn processInstruction(self: *Ship, action: u8, value: i32) void {
        switch (action) {
            'N' => self.waypoint_y += value,
            'S' => self.waypoint_y -= value,
            'E' => self.waypoint_x += value,
            'W' => self.waypoint_x -= value,
            'L' => self.rotateWaypoint(-value),
            'R' => self.rotateWaypoint(value),
            'F' => {
                self.x += self.waypoint_x * value;
                self.y += self.waypoint_y * value;
            },
            else => {},
        }
    }

    fn rotateWaypoint(self: *Ship, degrees: i32) void {
        const d = @mod((degrees + 360), 360);
        switch (d) {
            90 => {
                const temp = self.waypoint_x;
                self.waypoint_x = self.waypoint_y;
                self.waypoint_y = -temp;
            },
            180 => {
                self.waypoint_x = -self.waypoint_x;
                self.waypoint_y = -self.waypoint_y;
            },
            270 => {
                const temp = self.waypoint_x;
                self.waypoint_x = -self.waypoint_y;
                self.waypoint_y = temp;
            },
            else => {},
        }
    }
};

fn abs(x: i32) i32 {
    return if (x < 0) -x else x;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var ship = Ship{ .x = 0, .y = 0, .waypoint_x = 10, .waypoint_y = 1 };

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const action = line[0];
        const value = try std.fmt.parseInt(i32, line[1 .. line.len], 10);
        ship.processInstruction(action, value);
    }

    const manhattan_distance = abs(ship.x) + abs(ship.y);
    std.debug.print("{}\n", .{manhattan_distance});
}