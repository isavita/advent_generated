const std = @import("std");

const MODULUS: u64 = 20201227;

fn findLoopSize(publicKey: u64) u64 {
    var value: u64 = 1;
    var loopSize: u64 = 0;
    while (true) {
        loopSize += 1;
        value = (value * 7) % MODULUS;
        if (value == publicKey) {
            return loopSize;
        }
    }
}

fn transform(subjectNumber: u64, loopSize: u64) u64 {
    var value: u64 = 1;
    var i: u64 = 0;
    while (i < loopSize) : (i += 1) {
        value = (value * subjectNumber) % MODULUS;
    }
    return value;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    var publicKeys: [2]u64 = undefined;
    var i: usize = 0;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        publicKeys[i] = try std.fmt.parseInt(u64, line, 10);
        i += 1;
    }

    const cardLoopSize = findLoopSize(publicKeys[0]);
    const encryptionKey = transform(publicKeys[1], cardLoopSize);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Encryption Key: {}\n", .{encryptionKey});
}