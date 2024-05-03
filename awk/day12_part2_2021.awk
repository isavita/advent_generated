#!/usr/bin/awk

BEGIN {
    # Initialize a hash to store the connections between caves
    while (getline < "input.txt") {
        split($0, caves, "-");
        connections[caves[1] "," caves[2]] = 1;
        connections[caves[2] "," caves[1]] = 1;
    }

    # Initialize a hash to store the size of each cave
    for (cave in connections) {
        split(cave, cave_parts, ",");
        cave_name = cave_parts[1];
        if (cave_name ~ /^[A-Z]+$/) {
            cave_sizes[cave_name] = "big";
        } else {
            cave_sizes[cave_name] = "small";
        }
    }

    # Initialize the count of paths
    path_count = 0;

    # Start the path generation
    generate_paths("start", "start", "", 0);
    print path_count;
}

function generate_paths(current_cave, original_cave, path, visited_twice) {
    # Add the current cave to the path
    path = path "," current_cave;

    # If we've reached the end cave, increment the path count
    if (current_cave == "end") {
        path_count++;
        return;
    }

    # Generate paths for each connected cave
    for (cave in connections) {
        split(cave, cave_parts, ",");
        next_cave = cave_parts[2];
        if (cave_parts[1] == current_cave) {
            # Skip the start cave
            if (next_cave == "start") {
                continue;
            }

            # Check if we've already visited this cave
            if (cave_sizes[next_cave] == "small" && index(path, "," next_cave ",") > 0) {
                # If we've already visited this cave, check if we've already visited a small cave twice
                if (visited_twice) {
                    continue;
                } else {
                    # Mark that we've visited a small cave twice
                    generate_paths(next_cave, original_cave, path, 1);
                }
            } else {
                generate_paths(next_cave, original_cave, path, visited_twice);
            }
        }
    }
}