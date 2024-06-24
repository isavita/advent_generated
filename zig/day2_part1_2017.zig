
const std = @import("std");

pub fn main() !void {
    // Open the input file
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var checksum: u32 = 0;
    var buf: [1024]u8 = undefined;

    // Read the file line by line
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var min: ?u32 = null;
        var max: ?u32 = null;

        var it = std.mem.split(u8, line, "\t");
        while (it.next()) |num_str| {
            const num = try std.fmt.parseInt(u32, num_str, 10);
            
            if (min == null or num < min.?) min = num;
            if (max == null or num > max.?) max = num;
        }

        if (min != null and max != null) {
            checksum += max.? - min.?;
        }
    }

    // Print the result
    const stdout = std.io.getStdOut().writer();
    try stdout.print("Checksum: {}\n", .{checksum});
}
