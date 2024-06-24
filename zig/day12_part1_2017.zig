
const std = @import("std");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var connections = AutoHashMap(u32, ArrayList(u32)).init(allocator);
    defer {
        var it = connections.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit();
        }
        connections.deinit();
    }

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var it = std.mem.split(u8, line, " <-> ");
        const program = try std.fmt.parseInt(u32, it.next().?, 10);
        var connected = ArrayList(u32).init(allocator);
        var connected_it = std.mem.split(u8, it.next().?, ", ");
        while (connected_it.next()) |conn| {
            try connected.append(try std.fmt.parseInt(u32, conn, 10));
        }
        try connections.put(program, connected);
    }

    var visited = AutoHashMap(u32, void).init(allocator);
    defer visited.deinit();

    try dfs(0, &connections, &visited);

    const count = visited.count();
    try std.io.getStdOut().writer().print("Programs in group 0: {}\n", .{count});
}

fn dfs(program: u32, connections: *AutoHashMap(u32, ArrayList(u32)), visited: *AutoHashMap(u32, void)) !void {
    if (visited.contains(program)) return;
    try visited.put(program, {});

    if (connections.get(program)) |connected| {
        for (connected.items) |conn| {
            try dfs(conn, connections, visited);
        }
    }
}
