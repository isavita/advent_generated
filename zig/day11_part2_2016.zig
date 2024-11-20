
const std = @import("std");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
const Allocator = std.mem.Allocator;

const Halves = struct {
    is_chip: bool,
    material: []const u8,
};

const State = struct {
    floors: [4]ArrayList(Halves),
    elevator_level: u8,
    steps: u32,
    allocator: Allocator,

    pub fn deinit(self: *State) void {
        for (self.floors) |*floor| {
            floor.deinit();
        }
    }

    pub fn clone(self: *const State) !State {
        var new_state = State{
            .floors = undefined,
            .elevator_level = self.elevator_level,
            .steps = self.steps + 1,
            .allocator = self.allocator,
        };

        for (0..4) |i| {
            new_state.floors[i] = try ArrayList(Halves).initCapacity(self.allocator, self.floors[i].items.len);
            for (self.floors[i].items) |item| {
                new_state.floors[i].appendAssumeCapacity(item);
            }
        }

        return new_state;
    }

    pub fn hashKey(self: *const State) !u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(std.mem.asBytes(&self.elevator_level));

        var gen_indices = AutoHashMap([]const u8, u8).init(self.allocator);
        defer gen_indices.deinit();

        var chip_indices = AutoHashMap([]const u8, u8).init(self.allocator);
        defer chip_indices.deinit();

        for (0..4) |floor| {
            for (self.floors[floor].items) |item| {
                if (item.is_chip) {
                    try chip_indices.put(item.material, @intCast(u8, floor));
                } else {
                    try gen_indices.put(item.material, @intCast(u8, floor));
                }
            }
        }

        var pairs = ArrayList([2]u8).init(self.allocator);
        defer pairs.deinit();

        var it = gen_indices.iterator();
        while (it.next()) |entry| {
            if (chip_indices.get(entry.key_ptr.*)) |chip_floor| {
                try pairs.append([_]u8{ entry.value_ptr.*, chip_floor });
            }
        }

        std.sort.sort([2]u8, pairs.items, {}, compPair);

        for (pairs.items) |pair| {
            hasher.update(std.mem.asBytes(&pair));
        }

        return hasher.final();
    }

    fn compPair(context: void, a: [2]u8, b: [2]u8) bool {
        _ = context;
        if (a[0] != b[0]) return a[0] < b[0];
        return a[1] < b[1];
    }

    pub fn isValid(self: *const State) bool {
        for (0..4) |floor| {
            var gens = AutoHashMap([]const u8, void).init(self.allocator);
            defer gens.deinit();

            for (self.floors[floor].items) |item| {
                if (!item.is_chip) {
                    gens.put(item.material, {}) catch {};
                }
            }

            if (gens.count() == 0) continue;

            for (self.floors[floor].items) |item| {
                if (item.is_chip and !gens.contains(item.material)) {
                    return false;
                }
            }
        }
        return true;
    }

    pub fn isDone(self: *const State) bool {
        var total_items: usize = 0;
        for (0..3) |i| {
            total_items += self.floors[i].items.len;
        }
        return total_items == 0;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const input = try std.fs.cwd().readFileAlloc(allocator, "input.txt", 1024 * 1024);
    defer allocator.free(input);

    const result = try rtgHellDay(allocator, input);
    std.debug.print("{}\n", .{result});
}

pub fn rtgHellDay(allocator: Allocator, input: []const u8) !u32 {
    var initial_state = try parseInitialState(allocator, input);
    defer initial_state.deinit();

    try initial_state.floors[0].append(Halves{ .is_chip = false, .material = "elerium" });
    try initial_state.floors[0].append(Halves{ .is_chip = true, .material = "elerium" });
    try initial_state.floors[0].append(Halves{ .is_chip = false, .material = "dilithium" });
    try initial_state.floors[0].append(Halves{ .is_chip = true, .material = "dilithium" });

    var queue = ArrayList(State).init(allocator);
    defer queue.deinit();

    var visited = AutoHashMap(u64, void).init(allocator);
    defer visited.deinit();

    try queue.append(initial_state);

    while (queue.items.len > 0) {
        var current = queue.orderedRemove(0);
        defer current.deinit();

        if (current.isDone()) {
            return current.steps;
        }

        const hash = try current.hashKey();
        if (visited.contains(hash)) continue;
        try visited.put(hash, {});

        var next_states = try getNextStates(&current);
        defer {
            for (next_states.items) |*state| {
                state.deinit();
            }
            next_states.deinit();
        }

        try queue.appendSlice(next_states.items);
    }

    return 0;
}

fn getNextStates(current: *State) !ArrayList(State) {
    var next_states = ArrayList(State).init(current.allocator);

    const elevator_levels = blk: {
        var levels = ArrayList(i8).init(current.allocator);
        if (current.elevator_level < 3) try levels.append(1);
        if (current.elevator_level > 0) try levels.append(-1);
        break :blk levels;
    };
    defer elevator_levels.deinit();

    for (elevator_levels.items) |level_diff| {
        const new_level = @intCast(u8, @as(i8, current.elevator_level) + level_diff);

        for (0..current.floors[current.elevator_level].items.len) |i| {
            var state = try current.clone();
            state.elevator_level = new_level;

            try state.floors[new_level].append(state.floors[current.elevator_level].items[i]);
            _ = state.floors[current.elevator_level].orderedRemove(i);

            if (state.isValid()) {
                try next_states.append(state);
            } else {
                state.deinit();
            }

            for (i + 1..current.floors[current.elevator_level].items.len) |j| {
                var two_item_state = try current.clone();
                two_item_state.elevator_level = new_level;

                try two_item_state.floors[new_level].append(two_item_state.floors[current.elevator_level].items[i]);
                try two_item_state.floors[new_level].append(two_item_state.floors[current.elevator_level].items[j]);

                _ = two_item_state.floors[current.elevator_level].orderedRemove(j);
                _ = two_item_state.floors[current.elevator_level].orderedRemove(i);

                if (two_item_state.isValid()) {
                    try next_states.append(two_item_state);
                } else {
                    two_item_state.deinit();
                }
            }
        }
    }

    return next_states;
}

fn parseInitialState(allocator: Allocator, input: []const u8) !State {
    var state = State{
        .floors = undefined,
        .elevator_level = 0,
        .steps = 0,
        .allocator = allocator,
    };

    for (0..4) |i| {
        state.floors[i] = ArrayList(Halves).init(allocator);
    }

    var lines = std.mem.tokenize(u8, input, "\n");
    var floor_index: usize = 0;
    while (lines.next()) |line| {
        var words = std.mem.tokenize(u8, line, " ,.");
        while (words.next()) |word| {
            if (std.mem.eql(u8, word, "generator")) {
                const material = words.peek() orelse continue;
                try state.floors[floor_index].append(Halves{
                    .is_chip = false,
                    .material = material,
                });
            } else if (std.mem.eql(u8, word, "microchip")) {
                const material = words.peek() orelse continue;
                const chip_material = material[0..std.mem.indexOf(u8, material, "-comp") orelse material.len];
                try state.floors[floor_index].append(Halves{
                    .is_chip = true,
                    .material = chip_material,
                });
            }
        }
        floor_index += 1;
    }

    return state;
}
