const std = @import("std");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var program = std.ArrayList(u32).init(allocator);
    defer program.deinit();

    var it = std.mem.split(u8, content, ",");
    while (it.next()) |num_str| {
        const num = try std.fmt.parseInt(u32, num_str, 10);
        try program.append(num);
    }

    program.items[1] = 12;
    program.items[2] = 2;

    var pc: usize = 0;
    while (true) {
        const opcode = program.items[pc];
        switch (opcode) {
            1 => {
                const a = program.items[program.items[pc + 1]];
                const b = program.items[program.items[pc + 2]];
                const dest = program.items[pc + 3];
                program.items[dest] = a + b;
            },
            2 => {
                const a = program.items[program.items[pc + 1]];
                const b = program.items[program.items[pc + 2]];
                const dest = program.items[pc + 3];
                program.items[dest] = a * b;
            },
            99 => break,
            else => unreachable,
        }
        pc += 4;
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{program.items[0]});
}