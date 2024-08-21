const std = @import("std");

fn evaluateExpression(expression: []const u8) u64 {
    var result: u64 = 0;
    var current_op: u8 = '+';
    var i: usize = 0;

    while (i < expression.len) {
        const char = expression[i];
        if (char == ' ') {
            i += 1;
            continue;
        } else if (char == '(') {
            var depth: usize = 1;
            var j: usize = i + 1;
            while (j < expression.len and depth > 0) {
                if (expression[j] == '(') {
                    depth += 1;
                } else if (expression[j] == ')') {
                    depth -= 1;
                }
                j += 1;
            }
            const sub_result = evaluateExpression(expression[i + 1 .. j - 1]);
            if (current_op == '+') {
                result += sub_result;
            } else if (current_op == '*') {
                result *= sub_result;
            }
            i = j;
        } else if (char == '+' or char == '*') {
            current_op = char;
            i += 1;
        } else {
            var num: u64 = 0;
            var j = i;
            while (j < expression.len and std.ascii.isDigit(expression[j])) {
                num = num * 10 + (expression[j] - '0');
                j += 1;
            }
            if (current_op == '+') {
                result += num;
            } else if (current_op == '*') {
                result *= num;
            }
            i = j;
        }
    }

    return result;
}

fn evaluateWithPrecedence(expression: []const u8) u64 {
    var result: u64 = 1;
    var temp_sum: u64 = 0;
    var i: usize = 0;

    while (i < expression.len) {
        const char = expression[i];
        if (char == ' ') {
            i += 1;
            continue;
        } else if (char == '(') {
            var depth: usize = 1;
            var j: usize = i + 1;
            while (j < expression.len and depth > 0) {
                if (expression[j] == '(') {
                    depth += 1;
                } else if (expression[j] == ')') {
                    depth -= 1;
                }
                j += 1;
            }
            const sub_result = evaluateWithPrecedence(expression[i + 1 .. j - 1]);
            if (temp_sum == 0) {
                temp_sum = sub_result;
            } else {
                temp_sum += sub_result;
            }
            i = j;
        } else if (char == '+') {
            i += 1;
        } else if (char == '*') {
            result *= temp_sum;
            temp_sum = 0;
            i += 1;
        } else {
            var num: u64 = 0;
            var j = i;
            while (j < expression.len and std.ascii.isDigit(expression[j])) {
                num = num * 10 + (expression[j] - '0');
                j += 1;
            }
            if (temp_sum == 0) {
                temp_sum = num;
            } else {
                temp_sum += num;
            }
            i = j;
        }
    }

    return result * temp_sum;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var total: u64 = 0;
    var total_with_precedence: u64 = 0;

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        total += evaluateExpression(line);
        total_with_precedence += evaluateWithPrecedence(line);
    }

    std.debug.print("Part 1: {}\n", .{total});
    std.debug.print("Part 2: {}\n", .{total_with_precedence});
}