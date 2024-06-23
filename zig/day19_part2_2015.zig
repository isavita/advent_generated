const std = @import("std");

const Replacement = struct {
    from: []const u8,
    to: []const u8,
};

fn compareReplacements(context: void, a: Replacement, b: Replacement) bool {
    _ = context;
    return a.to.len > b.to.len;
}

fn countStepsToElectron(allocator: std.mem.Allocator, replacements: []Replacement, start_molecule: []const u8) !usize {
    var prng = std.rand.DefaultPrng.init(0);
    const random = prng.random();

    var steps: usize = 0;
    var current_molecule = try allocator.dupe(u8, start_molecule);
    defer allocator.free(current_molecule);

    while (!std.mem.eql(u8, current_molecule, "e")) {
        var made_replacement = false;
        random.shuffle(Replacement, replacements);

        for (replacements) |replacement| {
            if (std.mem.indexOf(u8, current_molecule, replacement.to)) |index| {
                var new_molecule = try allocator.alloc(u8, current_molecule.len - replacement.to.len + replacement.from.len);
                defer allocator.free(new_molecule);

                std.mem.copy(u8, new_molecule, current_molecule[0..index]);
                std.mem.copy(u8, new_molecule[index..], replacement.from);
                std.mem.copy(u8, new_molecule[index + replacement.from.len..], current_molecule[index + replacement.to.len..]);

                allocator.free(current_molecule);
                current_molecule = try allocator.dupe(u8, new_molecule);
                steps += 1;
                made_replacement = true;
                break;
            }
        }

        if (!made_replacement) {
            // If we can't make a replacement, start over with a different random seed
            steps = 0;
            allocator.free(current_molecule);
            current_molecule = try allocator.dupe(u8, start_molecule);
            prng = std.rand.DefaultPrng.init(@intCast(std.time.milliTimestamp()));
        }
    }

    return steps;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var replacements = std.ArrayList(Replacement).init(allocator);
    defer {
        for (replacements.items) |replacement| {
            allocator.free(replacement.from);
            allocator.free(replacement.to);
        }
        replacements.deinit();
    }

    var buf: [1024]u8 = undefined;
    var medicine_molecule: []const u8 = undefined;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0) {
            // Empty line separates replacements from the molecule
            medicine_molecule = try allocator.dupe(u8, (try in_stream.readUntilDelimiterOrEof(&buf, '\n')).?);
            break;
        }
        var parts = std.mem.split(u8, line, " => ");
        const from = parts.next().?;
        const to = parts.next().?;
        try replacements.append(.{
            .from = try allocator.dupe(u8, from),
            .to = try allocator.dupe(u8, to),
        });
    }

    // Sort replacements by length of 'to' field, descending
    std.mem.sort(Replacement, replacements.items, {}, compareReplacements);

    const steps = try countStepsToElectron(allocator, replacements.items, medicine_molecule);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Fewest number of steps to produce the medicine molecule: {}\n", .{steps});

    // Clean up
    allocator.free(medicine_molecule);
}
