const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Read input from file
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    const sum = try sumAllNumbers(content);
    try std.io.getStdOut().writer().print("{d}\n", .{sum});
}

fn sumAllNumbers(json: []const u8) !i64 {
    var sum: i64 = 0;
    var i: usize = 0;
    while (i < json.len) {
        if (std.ascii.isDigit(json[i]) or json[i] == '-') {
            const start = i;
            i += 1;
            while (i < json.len and (std.ascii.isDigit(json[i]) or json[i] == '.')) : (i += 1) {}
            const number = try std.fmt.parseInt(i64, json[start..i], 10);
            sum += number;
        } else {
            i += 1;
        }
    }
    return sum;
}
