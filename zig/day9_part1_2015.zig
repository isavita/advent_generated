const std = @import("std");

const City = struct {
    name: []u8,
    connections: std.ArrayList(Connection),

    fn deinit(self: *City, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        self.connections.deinit();
    }
};

const Connection = struct {
    to: usize,
    distance: u32,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var cities = std.StringHashMap(usize).init(allocator);
    defer cities.deinit();

    var city_list = std.ArrayList(City).init(allocator);
    defer {
        for (city_list.items) |*city| {
            city.deinit(allocator);
        }
        city_list.deinit();
    }

    // Read input from file
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var lines = std.mem.split(u8, content, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        try parseLine(allocator, &cities, &city_list, line);
    }

    const shortest_distance = try findShortestRoute(allocator, city_list.items);
    try std.io.getStdOut().writer().print("{d}\n", .{shortest_distance});
}

fn parseLine(allocator: std.mem.Allocator, cities: *std.StringHashMap(usize), city_list: *std.ArrayList(City), line: []const u8) !void {
    var parts = std.mem.split(u8, line, " = ");
    const city_names = parts.next() orelse return error.InvalidInput;
    const distance_str = parts.next() orelse return error.InvalidInput;

    var city_parts = std.mem.split(u8, city_names, " to ");
    const city1 = city_parts.next() orelse return error.InvalidInput;
    const city2 = city_parts.next() orelse return error.InvalidInput;

    const distance = try std.fmt.parseInt(u32, distance_str, 10);

    const index1 = try getOrCreateCityIndex(allocator, cities, city_list, city1);
    const index2 = try getOrCreateCityIndex(allocator, cities, city_list, city2);

    try city_list.items[index1].connections.append(.{ .to = index2, .distance = distance });
    try city_list.items[index2].connections.append(.{ .to = index1, .distance = distance });
}

fn getOrCreateCityIndex(allocator: std.mem.Allocator, cities: *std.StringHashMap(usize), city_list: *std.ArrayList(City), city_name: []const u8) !usize {
    const result = try cities.getOrPut(city_name);
    if (!result.found_existing) {
        result.value_ptr.* = city_list.items.len;
        try city_list.append(.{
            .name = try allocator.dupe(u8, city_name),
            .connections = std.ArrayList(Connection).init(allocator),
        });
    }
    return result.value_ptr.*;
}

fn findShortestRoute(allocator: std.mem.Allocator, cities: []const City) !u32 {
    var visited = try allocator.alloc(bool, cities.len);
    defer allocator.free(visited);
    @memset(visited, false);

    var shortest: u32 = std.math.maxInt(u32);

    for (cities, 0..) |_, start| {
        const distance = try dfs(cities, visited, start, 1, 0);
        shortest = @min(shortest, distance);
    }

    return shortest;
}

fn dfs(cities: []const City, visited: []bool, current: usize, visited_count: usize, current_distance: u32) !u32 {
    visited[current] = true;
    defer visited[current] = false;

    if (visited_count == cities.len) {
        return current_distance;
    }

    var shortest: u32 = std.math.maxInt(u32);

    for (cities[current].connections.items) |connection| {
        if (!visited[connection.to]) {
            const distance = try dfs(cities, visited, connection.to, visited_count + 1, current_distance + connection.distance);
            shortest = @min(shortest, distance);
        }
    }

    return shortest;
}
