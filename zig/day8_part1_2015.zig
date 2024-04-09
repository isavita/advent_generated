const std = @import("std");

fn calculateMemoryLength(s: []const u8) usize {
    var length: usize = 0;
    var inEscape = false;
    var hexCount: usize = 0;

    var i: usize = 1;
    while (i < s.len - 1) {
        if (hexCount > 0) {
            hexCount -= 1;
        } else if (inEscape) {
            if (s[i] == 'x') {
                hexCount = 2;
            }
            inEscape = false;
            length += 1;
        } else if (s[i] == '\\') {
            inEscape = true;
        } else {
            length += 1;
        }
        i += 1;
    }
    return length;
}

pub fn main() !void {
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var totalDiff: usize = 0;
    var reader = std.io.bufferedReader(file.reader());
    var line_reader = reader.reader();

    var buffer: [1024]u8 = undefined;
    while (try line_reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        const codeLength = line.len;
        const memoryLength = calculateMemoryLength(line);
        totalDiff += codeLength - memoryLength;
    }

    std.debug.print("{}\n", .{totalDiff});
}