
const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;

pub fn main() !void {
    // Open the input file
    const file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    // Read the entire file content
    const content = try file.readToEndAlloc(std.heap.page_allocator, 1024 * 1024);
    defer std.heap.page_allocator.free(content);

    // Create a buffer to store the reduced polymer
    var buffer = try std.ArrayList(u8).initCapacity(std.heap.page_allocator, content.len);
    defer buffer.deinit();

    // Process the polymer
    for (content) |unit| {
        if (buffer.items.len > 0 and areReactive(buffer.items[buffer.items.len - 1], unit)) {
            _ = buffer.pop();
        } else {
            try buffer.append(unit);
        }
    }

    // Print the result
    const stdout = io.getStdOut().writer();
    try stdout.print("Remaining units: {}\n", .{buffer.items.len});
}

fn areReactive(a: u8, b: u8) bool {
    return (a ^ b) == 32;
}
