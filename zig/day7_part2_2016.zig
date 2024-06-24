
const std = @import("std");
const fs = std.fs;
const io = std.io;
const mem = std.mem;

fn hasABBA(slice: []const u8) bool {
    if (slice.len < 4) return false;
    var i: usize = 0;
    while (i < slice.len - 3) : (i += 1) {
        if (slice[i] == slice[i + 3] and slice[i + 1] == slice[i + 2] and slice[i] != slice[i + 1]) {
            return true;
        }
    }
    return false;
}

fn supportsTLS(ip: []const u8) bool {
    var hasOutsideABBA = false;
    var start: usize = 0;
    var inBrackets = false;

    for (ip, 0..) |c, i| {
        if (c == '[') {
            if (!inBrackets and hasABBA(ip[start..i])) {
                hasOutsideABBA = true;
            }
            inBrackets = true;
            start = i + 1;
        } else if (c == ']') {
            if (inBrackets and hasABBA(ip[start..i])) {
                return false;
            }
            inBrackets = false;
            start = i + 1;
        }
    }

    if (!inBrackets and hasABBA(ip[start..])) {
        hasOutsideABBA = true;
    }

    return hasOutsideABBA;
}

fn findABAs(slice: []const u8, abas: *std.ArrayList([3]u8)) !void {
    if (slice.len < 3) return;
    var i: usize = 0;
    while (i < slice.len - 2) : (i += 1) {
        if (slice[i] == slice[i + 2] and slice[i] != slice[i + 1]) {
            try abas.append([3]u8{ slice[i], slice[i + 1], slice[i + 2] });
        }
    }
}

fn supportsSSL(ip: []const u8, abas: *std.ArrayList([3]u8), babs: *std.ArrayList([3]u8)) !bool {
    var start: usize = 0;
    var inBrackets = false;

    for (ip, 0..) |c, i| {
        if (c == '[') {
            if (!inBrackets) try findABAs(ip[start..i], abas);
            inBrackets = true;
            start = i + 1;
        } else if (c == ']') {
            if (inBrackets) try findABAs(ip[start..i], babs);
            inBrackets = false;
            start = i + 1;
        }
    }

    if (!inBrackets) try findABAs(ip[start..], abas);

    for (abas.items) |aba| {
        for (babs.items) |bab| {
            if (aba[0] == bab[1] and aba[1] == bab[0]) {
                return true;
            }
        }
    }

    return false;
}

pub fn main() !void {
    var file = try fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var tlsCount: usize = 0;
    var sslCount: usize = 0;

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (supportsTLS(line)) {
            tlsCount += 1;
        }

        var abas = std.ArrayList([3]u8).init(allocator);
        var babs = std.ArrayList([3]u8).init(allocator);
        defer abas.deinit();
        defer babs.deinit();

        if (try supportsSSL(line, &abas, &babs)) {
            sslCount += 1;
        }
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Part 1: {d} IPs support TLS\n", .{tlsCount});
    try stdout.print("Part 2: {d} IPs support SSL\n", .{sslCount});
}
