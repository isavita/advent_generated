
const std = @import("std");

const Node = struct {
    children: u32,
    metadata: u32,
};

fn parseNode(numbers: []const u32, index: *usize) u32 {
    const node = Node{
        .children = numbers[index.*],
        .metadata = numbers[index.* + 1],
    };
    index.* += 2;

    var sum: u32 = 0;
    var i: u32 = 0;
    while (i < node.children) : (i += 1) {
        sum += parseNode(numbers, index);
    }

    i = 0;
    while (i < node.metadata) : (i += 1) {
        sum += numbers[index.*];
        index.* += 1;
    }

    return sum;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var numbers = std.ArrayList(u32).init(allocator);
    defer numbers.deinit();

    var it = std.mem.tokenize(u8, content, " \n");
    while (it.next()) |num_str| {
        const num = try std.fmt.parseInt(u32, num_str, 10);
        try numbers.append(num);
    }

    var index: usize = 0;
    const result = parseNode(numbers.items, &index);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Sum of all metadata entries: {}\n", .{result});
}
