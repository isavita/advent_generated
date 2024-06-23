const std = @import("std");

const MAX_HOUSES_PER_ELF = 50;
const PRESENT_MULTIPLIER = 11;

fn calculatePresents(house_number: usize) usize {
    var presents: usize = 0;
    var i: usize = 1;
    while (i * i <= house_number) : (i += 1) {
        if (house_number % i == 0) {
            if (house_number / i <= MAX_HOUSES_PER_ELF) {
                presents += i * PRESENT_MULTIPLIER;
            }
            if (i <= MAX_HOUSES_PER_ELF and i * i != house_number) {
                presents += (house_number / i) * PRESENT_MULTIPLIER;
            }
        }
    }
    return presents;
}

fn findLowestHouseNumber(target_presents: usize) usize {
    var house_number: usize = 1;
    while (true) : (house_number += 1) {
        const presents = calculatePresents(house_number);
        if (presents >= target_presents) {
            return house_number;
        }
    }
}

pub fn main() !void {
    // Read input from file
    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [20]u8 = undefined;
    const input = try in_stream.readUntilDelimiterOrEof(&buf, '\n');
    
    if (input) |line| {
        const target_presents = try std.fmt.parseInt(usize, line, 10);
        
        const lowest_house = findLowestHouseNumber(target_presents);

        const stdout = std.io.getStdOut().writer();
        try stdout.print("Lowest house number to get at least {} presents: {}\n", .{target_presents, lowest_house});
    } else {
        std.debug.print("Error: Input file is empty\n", .{});
    }
}
