const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const file_content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(file_content);

    var left_list = std.ArrayList(u32).init(allocator);
    defer left_list.deinit();
    var right_list = std.ArrayList(u32).init(allocator);
    defer right_list.deinit();

    var lines = std.mem.split(u8, file_content, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        var numbers = std.mem.split(u8, line, " ");
        
        while (numbers.next()) |num| {
            if (num.len == 0) continue;
            const left = try std.fmt.parseInt(u32, num, 10);
            try left_list.append(left);
            break;
        }
        while (numbers.next()) |num| {
            if (num.len == 0) continue;
            const right = try std.fmt.parseInt(u32, num, 10);
            try right_list.append(right);
            break;
        }
    }

    // Count occurrences in right_list
    var right_counts = std.AutoHashMap(u32, u32).init(allocator);
    defer right_counts.deinit();

    for (right_list.items) |num| {
        const count = right_counts.get(num) orelse 0;
        try right_counts.put(num, count + 1);
    }

    // Calculate similarity score
    var similarity_score: u32 = 0;
    for (left_list.items) |num| {
        const count = right_counts.get(num) orelse 0;
        similarity_score += num * count;
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Similarity score: {}\n", .{similarity_score});
}
