const std = @import("std");

const Ingredient = struct {
    name: []const u8,
    capacity: i32,
    durability: i32,
    flavor: i32,
    texture: i32,
    calories: i32,
};

fn calculateScore(ingredients: []const Ingredient, amounts: []const u32) u64 {
    var capacity: i32 = 0;
    var durability: i32 = 0;
    var flavor: i32 = 0;
    var texture: i32 = 0;

    for (ingredients, amounts) |ingredient, amount| {
        capacity += ingredient.capacity * @as(i32, @intCast(amount));
        durability += ingredient.durability * @as(i32, @intCast(amount));
        flavor += ingredient.flavor * @as(i32, @intCast(amount));
        texture += ingredient.texture * @as(i32, @intCast(amount));
    }

    if (capacity <= 0 or durability <= 0 or flavor <= 0 or texture <= 0) {
        return 0;
    }

    return @as(u64, @intCast(capacity)) * @as(u64, @intCast(durability)) * @as(u64, @intCast(flavor)) * @as(u64, @intCast(texture));
}

fn findBestScore(ingredients: []const Ingredient, amounts: []u32, index: usize, remaining: u32) u64 {
    if (index == ingredients.len - 1) {
        amounts[index] = remaining;
        return calculateScore(ingredients, amounts);
    }

    var best_score: u64 = 0;
    var i: u32 = 0;
    while (i <= remaining) : (i += 1) {
        amounts[index] = i;
        const score = findBestScore(ingredients, amounts, index + 1, remaining - i);
        if (score > best_score) {
            best_score = score;
        }
    }

    return best_score;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var ingredients = std.ArrayList(Ingredient).init(allocator);
    defer ingredients.deinit();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var parts = std.mem.split(u8, line, ": ");
        const name = parts.next().?;
        const properties = parts.next().?;

        var props = std.mem.split(u8, properties, ", ");
        const capacity = try std.fmt.parseInt(i32, props.next().?[9..], 10);
        const durability = try std.fmt.parseInt(i32, props.next().?[11..], 10);
        const flavor = try std.fmt.parseInt(i32, props.next().?[7..], 10);
        const texture = try std.fmt.parseInt(i32, props.next().?[8..], 10);
        const calories = try std.fmt.parseInt(i32, props.next().?[9..], 10);

        try ingredients.append(Ingredient{
            .name = try allocator.dupe(u8, name),
            .capacity = capacity,
            .durability = durability,
            .flavor = flavor,
            .texture = texture,
            .calories = calories,
        });
    }

    var amounts = try allocator.alloc(u32, ingredients.items.len);
    defer allocator.free(amounts);

    const best_score = findBestScore(ingredients.items, amounts, 0, 100);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("The highest-scoring cookie has a score of {}\n", .{best_score});

    for (ingredients.items) |ingredient| {
        allocator.free(ingredient.name);
    }
}
