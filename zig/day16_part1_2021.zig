
const std = @import("std");

fn hexToBin(hex: []const u8, bin: []u8) void {
    const lut = "0000000100100011010001010110011110001001101010111100110111101111";
    for (hex, 0..) |h, i| {
        const idx = (if (h <= '9') h - '0' else h - 'A' + 10) * 4;
        @memcpy(bin[i * 4 .. i * 4 + 4], lut[idx .. idx + 4]);
    }
}

fn parsePacket(bin: []const u8, idx: *usize) u64 {
    const version = @as(u64, bin[idx.*] - '0') << 2 | @as(u64, bin[idx.* + 1] - '0') << 1 | @as(u64, bin[idx.* + 2] - '0');
    const typeID = @as(u64, bin[idx.* + 3] - '0') << 2 | @as(u64, bin[idx.* + 4] - '0') << 1 | @as(u64, bin[idx.* + 5] - '0');
    idx.* += 6;

    if (typeID == 4) {
        while (bin[idx.*] == '1') idx.* += 5;
        idx.* += 5;
        return version;
    }

    var versionSum: u64 = version;
    const lengthTypeID = bin[idx.*] - '0';
    idx.* += 1;

    if (lengthTypeID == 0) {
        var subPacketLength: usize = 0;
        for (0..15) |_| {
            subPacketLength = (subPacketLength << 1) | @as(usize, bin[idx.*] - '0');
            idx.* += 1;
        }
        const endIdx = idx.* + subPacketLength;
        while (idx.* < endIdx) {
            versionSum += parsePacket(bin, idx);
        }
    } else {
        var numSubPackets: usize = 0;
        for (0..11) |_| {
            numSubPackets = (numSubPackets << 1) | @as(usize, bin[idx.*] - '0');
            idx.* += 1;
        }
        for (0..numSubPackets) |_| {
            versionSum += parsePacket(bin, idx);
        }
    }
    return versionSum;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const hex = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(hex);

    const bin = try allocator.alloc(u8, hex.len * 4);
    defer allocator.free(bin);

    hexToBin(hex, bin);

    var idx: usize = 0;
    const versionSum = parsePacket(bin, &idx);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d}\n", .{versionSum});
}
