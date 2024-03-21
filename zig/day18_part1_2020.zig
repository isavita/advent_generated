const std = @import("std");

fn parseExpression(input: []const u8) i64 {
    var result: i64 = 0;
    var op: ?u8 = null;
    var num: i64 = 0;
    var i: usize = 0;

    while (i < input.len) {
        const c = input[i];
        if (c == ' ') {
            i += 1;
            continue;
        } else if (c >= '0' and c <= '9') {
            num = num * 10 + (c - '0');
        } else if (c == '+' or c == '*') {
            if (op) |op_| {
                if (op_ == '+') {
                    result += num;
                } else {
                    result *= num;
                }
            } else {
                result = num;
            }
            op = c;
            num = 0;
        } else if (c == '(') {
            var depth: usize = 1;
            i += 1;
            var start = i;
            while (depth > 0) {
                if (input[i] == '(') {
                    depth += 1;
                } else if (input[i] == ')') {
                    depth -= 1;
                }
                i += 1;
            }
            num = parseExpression(input[start .. i - 1]);
        } else if (c == ')') {
            if (op) |op_| {
                if (op_ == '+') {
                    result += num;
                } else {
                    result *= num;
                }
            } else {
                result = num;
            }
            return result;
        }
        i += 1;
    }

    if (op) |op_| {
        if (op_ == '+') {
            result += num;
        } else {
            result *= num;
        }
    } else {
        result = num;
    }

    return result;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var total: i64 = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const result = parseExpression(line);
        total += result;
    }

    std.debug.print("{}\n", .{total});
}