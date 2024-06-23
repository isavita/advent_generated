const std = @import("std");

const Item = enum {
    children,
    cats,
    samoyeds,
    pomeranians,
    akitas,
    vizslas,
    goldfish,
    trees,
    cars,
    perfumes,
};

const ItemCount = struct {
    item: Item,
    count: i32,
};

const AuntSue = struct {
    number: u32,
    items: std.ArrayList(ItemCount),
};

fn parseItem(item_str: []const u8) !Item {
    inline for (@typeInfo(Item).Enum.fields) |field| {
        if (std.mem.eql(u8, item_str, field.name)) {
            return @field(Item, field.name);
        }
    }
    return error.InvalidItem;
}

fn isMatchingItem(item: ItemCount, reading: ItemCount) bool {
    return switch (item.item) {
        .cats, .trees => item.count > reading.count,
        .pomeranians, .goldfish => item.count < reading.count,
        else => item.count == reading.count,
    };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var aunts = std.ArrayList(AuntSue).init(allocator);
    defer {
        for (aunts.items) |aunt| {
            aunt.items.deinit();
        }
        aunts.deinit();
    }

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var parts = std.mem.split(u8, line, ": ");
        const sue_part = parts.next() orelse continue;
        const sue_number = try std.fmt.parseInt(u32, sue_part[4..], 10);

        var items = std.ArrayList(ItemCount).init(allocator);
        
        var item_parts = parts.rest();
        var item_iter = std.mem.split(u8, item_parts, ", ");
        while (item_iter.next()) |item_str| {
            var kv = std.mem.split(u8, item_str, ": ");
            const item_name = kv.next() orelse continue;
            const count_str = kv.next() orelse continue;
            const item = try parseItem(item_name);
            const count = try std.fmt.parseInt(i32, count_str, 10);
            try items.append(ItemCount{ .item = item, .count = count });
        }

        try aunts.append(AuntSue{ .number = sue_number, .items = items });
    }

    const mfcsam_readings = [_]ItemCount{
        .{ .item = .children, .count = 3 },
        .{ .item = .cats, .count = 7 },
        .{ .item = .samoyeds, .count = 2 },
        .{ .item = .pomeranians, .count = 3 },
        .{ .item = .akitas, .count = 0 },
        .{ .item = .vizslas, .count = 0 },
        .{ .item = .goldfish, .count = 5 },
        .{ .item = .trees, .count = 3 },
        .{ .item = .cars, .count = 2 },
        .{ .item = .perfumes, .count = 1 },
    };

    var matching_sue: ?u32 = null;
    for (aunts.items) |aunt| {
        var matches = true;
        for (aunt.items.items) |item| {
            for (mfcsam_readings) |reading| {
                if (item.item == reading.item and !isMatchingItem(item, reading)) {
                    matches = false;
                    break;
                }
            }
            if (!matches) break;
        }
        if (matches) {
            matching_sue = aunt.number;
            break;
        }
    }

    const stdout = std.io.getStdOut().writer();
    if (matching_sue) |sue| {
        try stdout.print("The real Aunt Sue that got you the gift is number {}\n", .{sue});
    } else {
        try stdout.print("No matching Aunt Sue found.\n", .{});
    }
}
