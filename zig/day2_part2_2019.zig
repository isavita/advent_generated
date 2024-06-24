
const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var original = std.ArrayList(u32).init(allocator);
    defer original.deinit();

    var it = std.mem.split(u8, content, ",");
    while (it.next()) |num_str| {
        const num = try std.fmt.parseInt(u32, std.mem.trim(u8, num_str, &std.ascii.whitespace), 10);
        try original.append(num);
    }

    var memory: [200]u32 = undefined;
    const len = original.items.len;

    for (0..100) |noun| {
        for (0..100) |verb| {
            @memcpy(memory[0..len], original.items);
            memory[1] = @intCast(noun);
            memory[2] = @intCast(verb);
            if (execute(&memory, len) == 19690720) {
                const result = 100 * noun + verb;
                try std.io.getStdOut().writer().print("{d}\n", .{result});
                return;
            }
        }
    }
}

fn execute(memory: []u32, len: usize) u32 {
    var i: usize = 0;
    while (i < len) : (i += 4) {
        switch (memory[i]) {
            1 => memory[memory[i + 3]] = memory[memory[i + 1]] + memory[memory[i + 2]],
            2 => memory[memory[i + 3]] = memory[memory[i + 1]] * memory[memory[i + 2]],
            99 => return memory[0],
            else => unreachable,
        }
    }
    return memory[0];
}
