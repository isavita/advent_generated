
const std = @import("std");

fn dfs(node: usize, adj: []std.ArrayList(usize), visited: []bool) void {
    visited[node] = true;
    for (adj[node].items) |neighbor| {
        if (!visited[neighbor]) {
            dfs(neighbor, adj, visited);
        }
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var adj = std.ArrayList(std.ArrayList(usize)).init(allocator);
    defer {
        for (adj.items) |*list| {
            list.deinit();
        }
        adj.deinit();
    }

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();
    var buf: [1024]u8 = undefined;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var it = std.mem.split(u8, line, " <-> ");
        const from = try std.fmt.parseInt(usize, it.next().?, 10);

        while (adj.items.len <= from) {
            try adj.append(std.ArrayList(usize).init(allocator));
        }

        var connections = std.mem.split(u8, it.next().?, ", ");
        while (connections.next()) |conn| {
            const to = try std.fmt.parseInt(usize, conn, 10);
            while (adj.items.len <= to) {
                try adj.append(std.ArrayList(usize).init(allocator));
            }
            try adj.items[from].append(to);
            try adj.items[to].append(from);
        }
    }

    var visited = try allocator.alloc(bool, adj.items.len);
    defer allocator.free(visited);
    @memset(visited, false);

    var groups: usize = 0;
    for (0..adj.items.len) |node| {
        if (!visited[node]) {
            dfs(node, adj.items, visited);
            groups += 1;
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{groups});
}
