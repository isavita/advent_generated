const std = @import("std");

fn min(a: usize, b: usize) usize {
    return if (a < b) a else b;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const data = try file.readToEndAlloc(std.heap.page_allocator, 1024 * 1024);
    defer std.heap.page_allocator.free(data);

    const width: usize = 25;
    const height: usize = 6;
    const layerSize: usize = width * height;

    var minZeros: usize = layerSize + 1;
    var result: usize = 0;

    var i: usize = 0;
    while (i < data.len) : (i += layerSize) {
        const layer = data[i .. min(i + layerSize, data.len)];
        var zeroCount: usize = 0;
        var oneCount: usize = 0;
        var twoCount: usize = 0;

        for (layer) |pixel| {
            switch (pixel) {
                '0' => zeroCount += 1,
                '1' => oneCount += 1,
                '2' => twoCount += 1,
                else => {},
            }
        }

        if (zeroCount < minZeros) {
            minZeros = zeroCount;
            result = oneCount * twoCount;
        }
    }

    std.debug.print("{}\n", .{result});
}