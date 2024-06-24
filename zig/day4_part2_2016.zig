
const std = @import("std");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (isRealRoom(line)) {
            const decrypted_name = try decryptName(allocator, line);
            defer allocator.free(decrypted_name);
            if (std.mem.indexOf(u8, decrypted_name, "northpole object") != null) {
                const sector_id = getSectorID(line);
                try std.io.getStdOut().writer().print("{d}\n", .{sector_id});
                break;
            }
        }
    }
}

fn isRealRoom(room: []const u8) bool {
    const checksum_start = std.mem.lastIndexOf(u8, room, "[").?;
    const checksum = room[checksum_start + 1 .. room.len - 1];
    const name_end = std.mem.lastIndexOf(u8, room[0..checksum_start], "-").?;

    var letter_counts: [26]u16 = .{0} ** 26;
    for (room[0..name_end]) |c| {
        if (c >= 'a' and c <= 'z') {
            letter_counts[c - 'a'] += 1;
        }
    }

    var top_letters: [5]u8 = undefined;
    var i: usize = 0;
    while (i < 5) : (i += 1) {
        var max_count: u16 = 0;
        var max_letter: u8 = 0;
        for (letter_counts, 0..) |count, j| {
            if (count > max_count or (count == max_count and @as(u8, @intCast(j)) + 'a' < max_letter)) {
                max_count = count;
                max_letter = @as(u8, @intCast(j)) + 'a';
            }
        }
        top_letters[i] = max_letter;
        letter_counts[max_letter - 'a'] = 0;
    }

    return std.mem.eql(u8, &top_letters, checksum);
}

fn getSectorID(room: []const u8) u32 {
    const sector_start = std.mem.lastIndexOf(u8, room, "-").? + 1;
    const sector_end = std.mem.indexOf(u8, room[sector_start..], "[").? + sector_start;
    return std.fmt.parseInt(u32, room[sector_start..sector_end], 10) catch unreachable;
}

fn decryptName(allocator: std.mem.Allocator, room: []const u8) ![]u8 {
    const sector_id = getSectorID(room);
    const name_end = std.mem.lastIndexOf(u8, room, "-").?;

    var decrypted = try allocator.alloc(u8, name_end);
    for (room[0..name_end], 0..) |c, i| {
        if (c == '-') {
            decrypted[i] = ' ';
        } else {
            decrypted[i] = 'a' + @as(u8, @intCast((c - 'a' + sector_id) % 26));
        }
    }
    return decrypted;
}
