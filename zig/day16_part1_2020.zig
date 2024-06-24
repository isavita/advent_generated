
const std = @import("std");

const Rule = struct {
    name: []const u8,
    ranges: [2][2]u32,

    fn isValid(self: *const Rule, value: u32) bool {
        return (value >= self.ranges[0][0] and value <= self.ranges[0][1]) or
               (value >= self.ranges[1][0] and value <= self.ranges[1][1]);
    }
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var rules = std.ArrayList(Rule).init(allocator);
    var error_rate: u32 = 0;
    var scanning_rules = true;

    var buf: [256]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0) continue;
        if (std.mem.startsWith(u8, line, "your ticket:") or std.mem.startsWith(u8, line, "nearby tickets:")) {
            scanning_rules = false;
            continue;
        }

        if (scanning_rules) {
            if (parseRule(line)) |rule| {
                try rules.append(rule);
            }
        } else {
            var it = std.mem.split(u8, line, ",");
            while (it.next()) |value_str| {
                const value = try std.fmt.parseInt(u32, value_str, 10);
                if (!isValidForAnyRule(value, rules.items)) {
                    error_rate += value;
                }
            }
        }
    }

    try std.io.getStdOut().writer().print("{}\n", .{error_rate});
}

fn parseRule(line: []const u8) ?Rule {
    var it = std.mem.split(u8, line, ": ");
    const name = it.next() orelse return null;
    const ranges_str = it.next() orelse return null;

    var ranges_it = std.mem.split(u8, ranges_str, " or ");
    var ranges: [2][2]u32 = undefined;

    inline for (0..2) |i| {
        const range_str = ranges_it.next() orelse return null;
        var range_it = std.mem.split(u8, range_str, "-");
        ranges[i][0] = std.fmt.parseInt(u32, range_it.next() orelse return null, 10) catch return null;
        ranges[i][1] = std.fmt.parseInt(u32, range_it.next() orelse return null, 10) catch return null;
    }

    return Rule{ .name = name, .ranges = ranges };
}

fn isValidForAnyRule(value: u32, rules: []const Rule) bool {
    for (rules) |*rule| {
        if (rule.isValid(value)) return true;
    }
    return false;
}
