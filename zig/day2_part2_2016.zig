
const std = @import("std");

pub fn main() !void {
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();
    const reader = file.reader();
    
    const keypad1 = [_][3]u8{
        .{1,2,3},
        .{4,5,6},
        .{7,8,9},
    };
    const keypad2 = [_][5]u8{
        .{' ',' ','1',' ',' '},
        .{' ','2','3','4',' '},
        .{'5','6','7','8','9'},
        .{' ','A','B','C',' '},
        .{' ',' ','D',' ',' '},
    };
    
    var code1: [32]u8 = undefined;
    var code2: [32]u8 = undefined;
    var len1: usize = 0;
    var len2: usize = 0;
    
    var pos1_r: usize = 1;
    var pos1_c: usize = 1;
    var pos2_r: usize = 2;
    var pos2_c: usize = 0;
    
    var buf: [1024]u8 = undefined;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        for (line) |move| {
            switch (move) {
                'U' => {
                    if (pos1_r > 0) pos1_r -= 1;
                    if (pos2_r > 0 and keypad2[pos2_r - 1][pos2_c] != ' ') pos2_r -= 1;
                },
                'D' => {
                    if (pos1_r < 2) pos1_r += 1;
                    if (pos2_r < 4 and keypad2[pos2_r + 1][pos2_c] != ' ') pos2_r += 1;
                },
                'L' => {
                    if (pos1_c > 0) pos1_c -= 1;
                    if (pos2_c > 0 and keypad2[pos2_r][pos2_c - 1] != ' ') pos2_c -= 1;
                },
                'R' => {
                    if (pos1_c < 2) pos1_c += 1;
                    if (pos2_c < 4 and keypad2[pos2_r][pos2_c + 1] != ' ') pos2_c += 1;
                },
                else => {},
            }
        }
        code1[len1] = '0' + keypad1[pos1_r][pos1_c];
        len1 += 1;
        code2[len2] = keypad2[pos2_r][pos2_c];
        len2 += 1;
    }
    
    const out = std.io.getStdOut().writer();
    try out.writeAll(code1[0..len1]);
    try out.writeAll("\n");
    try out.writeAll(code2[0..len2]);
    try out.writeAll("\n");
}
