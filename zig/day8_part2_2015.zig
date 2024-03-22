const std = @import("std");

fn part1(input: []const u8) !usize {
    var total_code_chars: usize = 0;
    var total_memory_chars: usize = 0;
    var current_line_index: usize = 0;

    while (current_line_index < input.len) {
        var line_end_index = current_line_index;
        while (line_end_index < input.len and input[line_end_index] != '\n') {
            line_end_index += 1;
        }

        const line = input[current_line_index .. line_end_index];
        total_code_chars += line.len;

        var memory_chars: usize = 0;
        var i: usize = 1;
        while (i < line.len - 1) {
            if (line[i] == '\\') {
                if (line[i + 1] == 'x') {
                    i += 3;
                } else {
                    i += 2;
                }
            } else {
                i += 1;
            }
            memory_chars += 1;
        }
        total_memory_chars += memory_chars;

        current_line_index = line_end_index + 1;
    }

    return total_code_chars - total_memory_chars;
}

fn part2(input: []const u8) !usize {
    var total_encoded_chars: usize = 0;
    var total_code_chars: usize = 0;
    var current_line_index: usize = 0;

    while (current_line_index < input.len) {
        var line_end_index = current_line_index;
        while (line_end_index < input.len and input[line_end_index] != '\n') {
            line_end_index += 1;
        }

        const line = input[current_line_index .. line_end_index];
        total_code_chars += line.len;

        var encoded_chars: usize = 2;
        for (line) |c| {
            if (c == '\\' or c == '"') {
                encoded_chars += 2;
            } else {
                encoded_chars += 1;
            }
        }
        total_encoded_chars += encoded_chars;

        current_line_index = line_end_index + 1;
    }

    return total_encoded_chars - total_code_chars;
}

pub fn main() !void {
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const input = try file.readToEndAlloc(std.heap.page_allocator, 1024 * 1024);
    defer std.heap.page_allocator.free(input);

    std.debug.print("Part 1: {}\n", .{try part1(input)});
    std.debug.print("Part 2: {}\n", .{try part2(input)});
}