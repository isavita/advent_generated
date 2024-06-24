
const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var contains = std.StringHashMap(std.ArrayList([]const u8)).init(allocator);
    defer {
        var it = contains.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit();
        }
        contains.deinit();
    }

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var parts = std.mem.split(u8, line, " bags contain ");
        const container = parts.next().?;
        const contained = parts.next().?;

        if (std.mem.eql(u8, contained, "no other bags.")) continue;

        var bags = std.mem.split(u8, contained, ", ");
        while (bags.next()) |bag| {
            var words = std.mem.split(u8, bag, " ");
            _ = words.next();
            const color1 = words.next().?;
            const color2 = words.next().?;
            const bagName = try std.fmt.allocPrint(allocator, "{s} {s}", .{ color1, color2 });

            var entry = try contains.getOrPut(bagName);
            if (!entry.found_existing) {
                entry.value_ptr.* = std.ArrayList([]const u8).init(allocator);
            }
            try entry.value_ptr.append(try allocator.dupe(u8, container));
        }
    }

    var seen = std.StringHashMap(void).init(allocator);
    defer seen.deinit();

    try dfs("shiny gold", &contains, &seen);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{seen.count()});
}

fn dfs(bag: []const u8, contains: *std.StringHashMap(std.ArrayList([]const u8)), seen: *std.StringHashMap(void)) !void {
    if (contains.get(bag)) |outers| {
        for (outers.items) |outer| {
            if (!seen.contains(outer)) {
                try seen.put(outer, {});
                try dfs(outer, contains, seen);
            }
        }
    }
}
