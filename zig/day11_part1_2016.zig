
const std = @import("std");
const print = std.debug.print;
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

const Item = struct {
    element: []const u8,
    is_generator: bool,
};

const State = struct {
    elevator: usize,
    floors: [4]ArrayList(Item),
    steps: usize,

    fn init(allocator: std.mem.Allocator) State {
        var state = State{
            .elevator = 0,
            .floors = undefined,
            .steps = 0,
        };
        for (state.floors) |*floor| {
            floor.* = ArrayList(Item).init(allocator);
        }
        return state;
    }

    fn deinit(self: *State) void {
        for (self.floors) |*floor| {
            floor.deinit();
        }
    }

    fn clone(self: *const State, allocator: std.mem.Allocator) !State {
        var new_state = State.init(allocator);
        new_state.elevator = self.elevator;
        new_state.steps = self.steps;
        for (self.floors, 0..) |floor, i| {
            try new_state.floors[i].appendSlice(floor.items);
        }
        return new_state;
    }

    fn isValid(self: *const State) bool {
        for (self.floors) |floor| {
            var has_generator = false;
            for (floor.items) |item| {
                if (item.is_generator) {
                    has_generator = true;
                    break;
                }
            }
            if (has_generator) {
                for (floor.items) |item| {
                    if (!item.is_generator) {
                        var has_own_generator = false;
                        for (floor.items) |other_item| {
                            if (other_item.is_generator and std.mem.eql(u8, other_item.element, item.element)) {
                                has_own_generator = true;
                                break;
                            }
                        }
                        if (!has_own_generator) {
                            return false;
                        }
                    }
                }
            }
        }
        return true;
    }

    fn isComplete(self: *const State) bool {
        return self.floors[3].items.len == self.floors[0].items.len + self.floors[1].items.len + self.floors[2].items.len + self.floors[3].items.len;
    }

    fn hash(self: *const State) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(&[1]u8{@intCast(u8, self.elevator)});
        for (self.floors) |floor| {
            var generators: u64 = 0;
            var microchips: u64 = 0;
            for (floor.items) |item| {
                const bit = @as(u64, 1) << @intCast(u6, item.element[0] - 'A');
                if (item.is_generator) {
                    generators |= bit;
                } else {
                    microchips |= bit;
                }
            }
            hasher.update(std.mem.asBytes(&generators));
            hasher.update(std.mem.asBytes(&microchips));
        }
        return hasher.final();
    }
};

fn solve(initial_state: State, allocator: std.mem.Allocator) !usize {
    var queue = ArrayList(State).init(allocator);
    defer queue.deinit();
    try queue.append(initial_state);

    var seen = AutoHashMap(u64, void).init(allocator);
    defer seen.deinit();

    while (queue.items.len > 0) {
        var current_state = queue.orderedRemove(0);
        defer current_state.deinit();

        if (current_state.isComplete()) {
            return current_state.steps;
        }

        const state_hash = current_state.hash();
        if (seen.contains(state_hash)) {
            continue;
        }
        try seen.put(state_hash, {});

        const possible_floors = [_]isize{ -1, 1 };
        for (possible_floors) |floor_delta| {
            const new_floor = @intCast(usize, @intCast(isize, current_state.elevator) + floor_delta);
            if (new_floor >= 4) {
                continue;
            }

            var items_to_move = ArrayList(usize).init(allocator);
            defer items_to_move.deinit();

            for (current_state.floors[current_state.elevator].items, 0..) |_, i| {
                try items_to_move.append(i);
                if (items_to_move.items.len == 2) break;
            }

            var i: usize = 0;
            while (i < items_to_move.items.len) : (i += 1) {
                var j: usize = i;
                while (j < items_to_move.items.len) : (j += 1) {
                    var new_state = try current_state.clone(allocator);
                    new_state.elevator = new_floor;
                    new_state.steps += 1;

                    const item1 = new_state.floors[current_state.elevator].orderedRemove(items_to_move.items[i]);
                    try new_state.floors[new_floor].append(item1);

                    if (i != j) {
                        const item2 = new_state.floors[current_state.elevator].orderedRemove(items_to_move.items[j] - 1);
                        try new_state.floors[new_floor].append(item2);
                    }

                    if (new_state.isValid()) {
                        try queue.append(new_state);
                    } else {
                        new_state.deinit();
                    }
                }
            }
        }
    }

    return error.NoSolutionFound;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var initial_state = State.init(allocator);
    defer initial_state.deinit();

    var buf: [1024]u8 = undefined;
    var floor: usize = 0;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var it = std.mem.split(u8, line, " ");
        while (it.next()) |word| {
            if (std.mem.indexOf(u8, word, "generator") != null) {
                const element = word[0 .. word.len - 10];
                try initial_state.floors[floor].append(.{ .element = element, .is_generator = true });
            } else if (std.mem.indexOf(u8, word, "microchip") != null) {
                const element = word[0 .. word.len - 11];
                try initial_state.floors[floor].append(.{ .element = element, .is_generator = false });
            }
        }
        floor += 1;
    }

    const result = try solve(initial_state, allocator);
    print("Minimum number of steps: {}\n", .{result});
}
