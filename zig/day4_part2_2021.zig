
const std = @import("std");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

const Board = struct {
    nums: [25]u8,
    marked: [25]bool,

    fn init(nums: [25]u8) Board {
        return .{ .nums = nums, .marked = [_]bool{false} ** 25 };
    }

    fn mark(self: *Board, num: u8) bool {
        for (self.nums, 0..) |n, i| {
            if (n == num) {
                self.marked[i] = true;
                return self.checkWin(i);
            }
        }
        return false;
    }

    fn checkWin(self: *Board, index: usize) bool {
        const row = index / 5;
        const col = index % 5;
        var row_win = true;
        var col_win = true;
        for (0..5) |i| {
            row_win = row_win and self.marked[row * 5 + i];
            col_win = col_win and self.marked[i * 5 + col];
        }
        return row_win or col_win;
    }

    fn score(self: *Board) u32 {
        var sum: u32 = 0;
        for (self.nums, self.marked) |n, m| {
            if (!m) sum += n;
        }
        return sum;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var result = try solve(allocator, content);
    try std.io.getStdOut().writer().print("{d}\n", .{result});
}

fn solve(allocator: std.mem.Allocator, input: []const u8) !u32 {
    var nums = ArrayList(u8).init(allocator);
    defer nums.deinit();

    var boards = ArrayList(Board).init(allocator);
    defer boards.deinit();

    var it = std.mem.split(u8, input, "\n\n");
    const num_str = it.next().?;
    var num_it = std.mem.tokenize(u8, num_str, ",");
    while (num_it.next()) |n| {
        try nums.append(try std.fmt.parseInt(u8, n, 10));
    }

    while (it.next()) |board_str| {
        var board_nums: [25]u8 = undefined;
        var board_it = std.mem.tokenize(u8, board_str, " \n");
        for (0..25) |i| {
            board_nums[i] = try std.fmt.parseInt(u8, board_it.next().?, 10);
        }
        try boards.append(Board.init(board_nums));
    }

    var last_score: u32 = 0;
    var won = AutoHashMap(usize, void).init(allocator);
    defer won.deinit();

    for (nums.items) |n| {
        for (boards.items, 0..) |*board, i| {
            if (won.contains(i)) continue;
            if (board.mark(n)) {
                last_score = board.score() * n;
                try won.put(i, {});
            }
        }
    }

    return last_score;
}
