
const std = @import("std");
const print = std.debug.print;
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

const Bot = struct {
    chips: [2]u8,
    chip_count: u8,
    low_dest: Destination,
    high_dest: Destination,
};

const Destination = struct {
    is_bot: bool,
    id: u8,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var bots = AutoHashMap(u8, Bot).init(allocator);
    defer bots.deinit();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (std.mem.startsWith(u8, line, "value")) {
            var it = std.mem.split(u8, line, " ");
            _ = it.next();
            const value = try std.fmt.parseInt(u8, it.next().?, 10);
            _ = it.next();
            _ = it.next();
            _ = it.next();
            const bot_id = try std.fmt.parseInt(u8, it.next().?, 10);
            try addChipToBot(&bots, bot_id, value);
        } else if (std.mem.startsWith(u8, line, "bot")) {
            var it = std.mem.split(u8, line, " ");
            _ = it.next();
            const bot_id = try std.fmt.parseInt(u8, it.next().?, 10);
            _ = it.next();
            _ = it.next();
            _ = it.next();
            const low_dest = parseDestination(it.next().?, it.next().?);
            _ = it.next();
            _ = it.next();
            const high_dest = parseDestination(it.next().?, it.next().?);

            var bot = try bots.getOrPut(bot_id);
            if (!bot.found_existing) {
                bot.value_ptr.* = Bot{ .chips = undefined, .chip_count = 0, .low_dest = low_dest, .high_dest = high_dest };
            } else {
                bot.value_ptr.low_dest = low_dest;
                bot.value_ptr.high_dest = high_dest;
            }
        }
    }

    var it = bots.iterator();
    while (it.next()) |entry| {
        const bot_id = entry.key_ptr.*;
        const bot = entry.value_ptr;
        if (bot.chip_count == 2 and
            ((bot.chips[0] == 61 and bot.chips[1] == 17) or
            (bot.chips[0] == 17 and bot.chips[1] == 61)))
        {
            print("Bot {} is responsible for comparing value-61 and value-17 microchips.\n", .{bot_id});
            return;
        }
    }
}

fn addChipToBot(bots: *AutoHashMap(u8, Bot), bot_id: u8, value: u8) !void {
    var bot = try bots.getOrPut(bot_id);
    if (!bot.found_existing) {
        bot.value_ptr.* = Bot{ .chips = undefined, .chip_count = 0, .low_dest = undefined, .high_dest = undefined };
    }
    bot.value_ptr.chips[bot.value_ptr.chip_count] = value;
    bot.value_ptr.chip_count += 1;
}

fn parseDestination(dest_type: []const u8, id_str: []const u8) Destination {
    const id = std.fmt.parseInt(u8, id_str, 10) catch unreachable;
    return Destination{ .is_bot = std.mem.eql(u8, dest_type, "bot"), .id = id };
}
