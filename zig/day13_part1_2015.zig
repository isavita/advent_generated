const std = @import("std");

const MAX_PEOPLE = 10;
const Person = struct {
    name: []u8,
    happiness: [MAX_PEOPLE]i32,

    fn deinit(self: *Person, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    var people = std.ArrayList(Person).init(allocator);
    defer {
        for (people.items) |*person| {
            person.deinit(allocator);
        }
        people.deinit();
    }

    var lines = std.mem.split(u8, content, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        try parseLine(allocator, &people, line);
    }

    const max_happiness = try findMaxHappiness(allocator, people.items);
    try std.io.getStdOut().writer().print("{d}\n", .{max_happiness});
}

fn parseLine(allocator: std.mem.Allocator, people: *std.ArrayList(Person), line: []const u8) !void {
    var words = std.mem.split(u8, line, " ");
    const person1 = words.next().?;
    _ = words.next(); // would
    const gain_or_lose = words.next().?;
    const amount_str = words.next().?;
    _ = words.next(); // happiness
    _ = words.next(); // units
    _ = words.next(); // by
    _ = words.next(); // sitting
    _ = words.next(); // next
    _ = words.next(); // to
    var person2 = words.next().?;
    person2 = person2[0..person2.len - 1]; // Remove the trailing dot

    const amount = try std.fmt.parseInt(i32, amount_str, 10);
    const happiness = if (std.mem.eql(u8, gain_or_lose, "gain")) amount else -amount;

    var person1_index = blk: {
        for (people.items, 0..) |p, i| {
            if (std.mem.eql(u8, p.name, person1)) break :blk i;
        }
        try people.append(.{ .name = try allocator.dupe(u8, person1), .happiness = [_]i32{0} ** MAX_PEOPLE });
        break :blk people.items.len - 1;
    };

    var person2_index = blk: {
        for (people.items, 0..) |p, i| {
            if (std.mem.eql(u8, p.name, person2)) break :blk i;
        }
        try people.append(.{ .name = try allocator.dupe(u8, person2), .happiness = [_]i32{0} ** MAX_PEOPLE });
        break :blk people.items.len - 1;
    };

    people.items[person1_index].happiness[person2_index] = happiness;
}

fn findMaxHappiness(allocator: std.mem.Allocator, people: []const Person) !i32 {
    var arrangement = try allocator.alloc(usize, people.len);
    defer allocator.free(arrangement);
    for (arrangement, 0..) |*a, i| a.* = i;

    var max_happiness: i32 = std.math.minInt(i32);
    while (true) {
        const happiness = calculateHappiness(people, arrangement);
        max_happiness = @max(max_happiness, happiness);

        if (!nextPermutation(arrangement)) break;
    }

    return max_happiness;
}

fn calculateHappiness(people: []const Person, arrangement: []const usize) i32 {
    var total: i32 = 0;
    const n = arrangement.len;
    for (arrangement, 0..) |person, i| {
        const next = arrangement[(i + 1) % n];
        const prev = arrangement[(i + n - 1) % n];
        total += people[person].happiness[next];
        total += people[person].happiness[prev];
    }
    return total;
}

fn nextPermutation(arr: []usize) bool {
    var i: usize = arr.len - 1;
    while (i > 0 and arr[i - 1] >= arr[i]) : (i -= 1) {}
    if (i == 0) return false;

    var j: usize = arr.len - 1;
    while (arr[j] <= arr[i - 1]) : (j -= 1) {}

    var temp = arr[i - 1];
    arr[i - 1] = arr[j];
    arr[j] = temp;

    j = arr.len - 1;
    while (i < j) : ({ i += 1; j -= 1; }) {
        temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }

    return true;
}
