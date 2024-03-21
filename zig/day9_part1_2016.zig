const std = @import("std");

fn decompress(input: []const u8) !usize {
    var i: usize = 0;
    var decompressed_len: usize = 0;

    while (i < input.len) {
        if (input[i] == '(') {
            i += 1;
            var end = i;
            while (input[end] != ')') {
                end += 1;
            }
            const marker = input[i .. end];
            i = end + 1;

            const x_index = std.mem.indexOfScalar(u8, marker, 'x').?;
            const len = try std.fmt.parseInt(usize, marker[0..x_index], 10);
            const repeat = try std.fmt.parseInt(usize, marker[x_index + 1 ..], 10);
            decompressed_len += len * repeat;
            i += len;
        } else {
            decompressed_len += 1;
            i += 1;
        }
    }

    return decompressed_len;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buffer: [1024 * 1024]u8 = undefined;
    const len = try file.readAll(&buffer);
    const decompressed_len = try decompress(buffer[0..len]);
    std.debug.print("Decompressed length: {}\n", .{decompressed_len});
}