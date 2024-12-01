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

    // Sort the lists in ascending order
    std.mem.sort(u32, left_list.items, {}, std.sort.asc(u32));
    std.mem.sort(u32, right_list.items, {}, std.sort.asc(u32));

    var total_distance: u32 = 0;
    for (left_list.items, right_list.items) |left, right| {
        const distance = if (left > right) left - right else right - left;
        total_distance += distance;
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Total distance: {}\n", .{total_distance});
}
