const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buffer: [1024]u8 = undefined;
    const n = try file.readAll(&buffer);
    const input = buffer[0..n];
    const steps = try std.fmt.parseInt(usize, input, 10);

    var current: usize = 0;
    var buffer_data = std.ArrayList(usize).init(allocator);
    defer buffer_data.deinit();

    buffer_data.append(0) catch unreachable;

    var i: usize = 1;
    while (i <= 2017) : (i += 1) {
        current = (current + steps) % buffer_data.items.len;
        buffer_data.insert(current + 1, i) catch unreachable;
        current += 1;
    }

    const result = buffer_data.items[(current + 1) % buffer_data.items.len];
    std.debug.print("The value after 2017 is: {}\n", .{result});
}