
const std = @import("std");

pub fn main() !void {
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const data = try file.readToEndAlloc(std.heap.page_allocator, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(data);

    const width: usize = 25;
    const height: usize = 6;
    const layerSize: usize = width * height;
    var finalImage: [layerSize]u8 = [_]u8{'2'} ** layerSize;

    var i: usize = 0;
    while (i < data.len) {
        const layerEnd = @min(i + layerSize, data.len);
        var j: usize = 0;
        while (i + j < layerEnd) {
            if (finalImage[j] == '2') {
                finalImage[j] = data[i + j];
            }
            j += 1;
        }
        i += layerSize;
    }

    try std.io.getStdOut().writer().print("Decoded image:\n", .{});
    i = 0;
    while (i < height) {
        var j: usize = 0;
        while (j < width) {
            const pixel = finalImage[i * width + j];
            if (pixel == '0') {
                try std.io.getStdOut().writer().print(" ", .{});
            } else if (pixel == '1') {
                try std.io.getStdOut().writer().print("#", .{});
            }
            j += 1;
        }
        try std.io.getStdOut().writer().print("\n", .{});
        i += 1;
    }
}
