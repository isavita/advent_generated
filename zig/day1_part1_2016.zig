const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator(); // This is of type `mem.Allocator`

    // Open the input file.
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    // After reading the entire file into memory
    const contents = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(contents);

    // Trim whitespace from the start and end of the file contents
    const trimmedContents = std.mem.trim(u8, contents, " \r\n\t");

    // Split the instructions and parse them.
    var x: i32 = 0;
    var y: i32 = 0;
    var direction: i32 = 0; // 0: North, 1: East, 2: South, 3: West

    // Split the file contents by commas to get each instruction.
    var tokens = std.mem.tokenizeAny(u8, trimmedContents, ", ");
    while (tokens.next()) |token| {
        const turn = token[0];
        const steps = std.fmt.parseInt(i32, token[1..], 10) catch 0;

        // Adjust direction based on turn.
        direction = if (turn == 'R') @mod(direction + 1, 4) else if (turn == 'L') @mod(direction + 3, 4) else direction;

        // Move in the current direction.
        switch (direction) {
            0 => y += steps, // North
            1 => x += steps, // East
            2 => y -= steps, // South
            3 => x -= steps, // West
            else => {},
        }
    }

    // Calculate and print the distance.
    const x_abs = try std.math.absInt(x);
    const y_abs = try std.math.absInt(y);
    const distance = x_abs + y_abs;
    std.debug.print("Distance to Easter Bunny HQ: {} blocks\n", .{distance});
}
