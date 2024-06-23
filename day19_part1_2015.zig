const std = @import("std");

const Replacement = struct {
    from: []const u8,
    to: []const u8,
};

fn countDistinctMolecules(allocator: std.mem.Allocator, replacements: []const Replacement, molecule: []const u8) !usize {
    var unique_molecules = std.ArrayList([]const u8).init(allocator);
    defer {
        for (unique_molecules.items) |m| {
            allocator.free(m);
        }
        unique_molecules.deinit();
    }

    for (replacements) |replacement| {
        var i: usize = 0;
        while (std.mem.indexOf(u8, molecule[i..], replacement.from)) |index| {
            const actual_index = i + index;
            var new_molecule = try allocator.alloc(u8, molecule.len - replacement.from.len + replacement.to.len);

            std.mem.copy(u8, new_molecule, molecule[0..actual_index]);
            std.mem.copy(u8, new_molecule[actual_index..], replacement.to);
            std.mem.copy(u8, new_molecule[actual_index + replacement.to.len..], molecule[actual_index + replacement.from.len..]);

            // Check if the new molecule is unique
            var is_unique = true;
            for (unique_molecules.items) |m| {
                if (std.mem.eql(u8, m, new_molecule)) {
                    is_unique = false;
                    break;
                }
            }

            if (is_unique) {
                try unique_molecules.append(new_molecule);
            } else {
                allocator.free(new_molecule);
            }

            i = actual_index + 1;
        }
    }

    return unique_molecules.items.len;
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
    var molecule: []const u8 = undefined;

    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (line.len == 0) {
            // Empty line separates replacements from the molecule
            molecule = try allocator.dupe(u8, (try in_stream.readUntilDelimiterOrEof(&buf, '\n')).?);
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

    const distinct_molecules = try countDistinctMolecules(allocator, replacements.items, molecule);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Number of distinct molecules: {}\n", .{distinct_molecules});

    // Clean up
    allocator.free(molecule);
}
