const std = @import("std");

const Scanner = struct {
    range: usize,
    position: usize,
    direction: i32,
};

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var firewall = std.AutoHashMap(usize, Scanner).init(allocator);
    defer firewall.deinit();

    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var iter = std.mem.tokenize(u8, line, ": ");
        const depth = try std.fmt.parseInt(usize, iter.next().?, 10);
        const range = try std.fmt.parseInt(usize, iter.next().?, 10);
        try firewall.put(depth, Scanner{
            .range = range,
            .position = 0,
            .direction = 1,
        });
    }

    var delay: usize = 0;
    while (true) {
        if (try passThrough(firewall, delay)) {
            break;
        }
        delay += 1;
    }

    std.debug.print("{}\n", .{delay});
}

fn passThrough(firewall: std.AutoHashMap(usize, Scanner), delay: usize) !bool {
    var iter = firewall.keyIterator();
    while (iter.next()) |depth| {
        const scanner = firewall.get(depth.*).?;
        if ((depth.* + delay) % (2 * (scanner.range - 1)) == 0) {
            return false;
        }
    }
    return true;
}