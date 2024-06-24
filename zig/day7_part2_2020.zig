
const std = @import("std");

const BagRule = struct {
    color: []const u8,
    count: u32,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var rules = std.StringHashMap([]BagRule).init(allocator);
    defer {
        var it = rules.iterator();
        while (it.next()) |entry| {
            allocator.free(entry.value_ptr.*);
        }
        rules.deinit();
    }

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var parts = std.mem.split(u8, line, " bags contain ");
        const container = parts.next().?;
        const contents = parts.next().?;

        if (std.mem.eql(u8, contents, "no other bags.")) continue;

        var rule_list = std.ArrayList(BagRule).init(allocator);

        var content_parts = std.mem.split(u8, contents, ", ");
        while (content_parts.next()) |part| {
            var rule_parts = std.mem.split(u8, part, " ");
            const count = try std.fmt.parseInt(u32, rule_parts.next().?, 10);
            const color = try std.mem.join(allocator, " ", &.{ rule_parts.next().?, rule_parts.next().? });
            try rule_list.append(.{ .color = color, .count = count });
        }

        try rules.put(try allocator.dupe(u8, container), try rule_list.toOwnedSlice());
    }

    const total_bags = try countBags("shiny gold", &rules, allocator) - 1;
    try std.io.getStdOut().writer().print("{d}\n", .{total_bags});
}

fn countBags(color: []const u8, rules: *const std.StringHashMap([]BagRule), allocator: std.mem.Allocator) !u32 {
    var count: u32 = 1;
    if (rules.get(color)) |rule_list| {
        for (rule_list) |rule| {
            count += rule.count * try countBags(rule.color, rules, allocator);
        }
    }
    return count;
}
