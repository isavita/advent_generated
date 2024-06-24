
const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;

fn hasAbba(slice: []const u8) bool {
    if (slice.len < 4) return false;
    var i: usize = 0;
    while (i < slice.len - 3) : (i += 1) {
        if (slice[i] == slice[i + 3] and
            slice[i + 1] == slice[i + 2] and
            slice[i] != slice[i + 1])
        {
            return true;
        }
    }
    return false;
}

fn supportsTls(ip: []const u8) bool {
    var hasOutsideAbba = false;
    var start: usize = 0;
    var inBrackets = false;

    for (ip, 0..) |c, i| {
        switch (c) {
            '[' => {
                if (!inBrackets) {
                    hasOutsideAbba = hasOutsideAbba or hasAbba(ip[start..i]);
                    inBrackets = true;
                    start = i + 1;
                }
            },
            ']' => {
                if (inBrackets) {
                    if (hasAbba(ip[start..i])) return false;
                    inBrackets = false;
                    start = i + 1;
                }
            },
            else => {},
        }
    }

    if (!inBrackets) {
        hasOutsideAbba = hasOutsideAbba or hasAbba(ip[start..]);
    }

    return hasOutsideAbba;
}

pub fn main() !void {
    var file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var count: usize = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (supportsTls(line)) {
            count += 1;
        }
    }

    const stdout = io.getStdOut().writer();
    try stdout.print("Number of IPs supporting TLS: {d}\n", .{count});
}
