
#!/usr/bin/awk -f

# Function to define the 5 rock shapes
# shapes[shape_idx, point_idx, "x" or "y"] stores relative coordinates
# shape_lengths[shape_idx] stores the number of points in the shape
function define_shapes( shapes, shape_lengths ) {
    # Shape 0: - (Horizontal Line)
    shape_lengths[0] = 4
    shapes[0, 1, "x"] = 0; shapes[0, 1, "y"] = 0
    shapes[0, 2, "x"] = 1; shapes[0, 2, "y"] = 0
    shapes[0, 3, "x"] = 2; shapes[0, 3, "y"] = 0
    shapes[0, 4, "x"] = 3; shapes[0, 4, "y"] = 0

    # Shape 1: + (Plus)
    shape_lengths[1] = 5
    shapes[1, 1, "x"] = 1; shapes[1, 1, "y"] = 0
    shapes[1, 2, "x"] = 0; shapes[1, 2, "y"] = 1
    shapes[1, 3, "x"] = 1; shapes[1, 3, "y"] = 1
    shapes[1, 4, "x"] = 2; shapes[1, 4, "y"] = 1
    shapes[1, 5, "x"] = 1; shapes[1, 5, "y"] = 2

    # Shape 2: L (Inverted L)
    shape_lengths[2] = 5
    shapes[2, 1, "x"] = 0; shapes[2, 1, "y"] = 0
    shapes[2, 2, "x"] = 1; shapes[2, 2, "y"] = 0
    shapes[2, 3, "x"] = 2; shapes[2, 3, "y"] = 0
    shapes[2, 4, "x"] = 2; shapes[2, 4, "y"] = 1
    shapes[2, 5, "x"] = 2; shapes[2, 5, "y"] = 2

    # Shape 3: | (Vertical Line)
    shape_lengths[3] = 4
    shapes[3, 1, "x"] = 0; shapes[3, 1, "y"] = 0
    shapes[3, 2, "x"] = 0; shapes[3, 2, "y"] = 1
    shapes[3, 3, "x"] = 0; shapes[3, 3, "y"] = 2
    shapes[3, 4, "x"] = 0; shapes[3, 4, "y"] = 3

    # Shape 4: Square
    shape_lengths[4] = 4
    shapes[4, 1, "x"] = 0; shapes[4, 1, "y"] = 0
    shapes[4, 2, "x"] = 1; shapes[4, 2, "y"] = 0
    shapes[4, 3, "x"] = 0; shapes[4, 3, "y"] = 1
    shapes[4, 4, "x"] = 1; shapes[4, 4, "y"] = 1
}

# Function to check if the current rock can move by dx, dy
# Uses global arrays: rock, chamber, num_points, WIDTH
# Returns 1 if move is possible, 0 otherwise
function can_move(dx, dy,   i, nx, ny) { # Local vars: i, nx, ny
    for (i = 1; i <= num_points; i++) {
        nx = rock[i, "x"] + dx
        ny = rock[i, "y"] + dy
        # Check boundaries (0 <= nx < WIDTH) and floor (ny > 0)
        # Check collision with settled rocks in chamber
        if (nx < 0 || nx >= WIDTH || ny <= 0 || (nx "," ny) in chamber) {
            return 0 # Cannot move
        }
    }
    return 1 # Can move
}

# Function to apply the move dx, dy to the current rock
# Uses global array: rock, num_points
function move_rock(dx, dy,   i) { # Local var: i
    for (i = 1; i <= num_points; i++) {
        rock[i, "x"] += dx
        rock[i, "y"] += dy
    }
}

# BEGIN block executes before reading any input line
BEGIN {
    # Define simulation parameters
    WIDTH = 7
    TOTAL_ROCKS = 2022
    INPUT_FILE = "input.txt"

    # Read the single line of jet pattern from the input file
    if ((getline jet_pattern < INPUT_FILE) <= 0) {
        printf "Error: Could not read from %s\n", INPUT_FILE > "/dev/stderr"
        exit 1
    }
    close(INPUT_FILE)
    jet_len = length(jet_pattern)

    # Define rock shapes
    define_shapes(shapes, shape_lengths)

    # Initialize chamber: Use associative array `chamber` like a set
    # Key format: "x,y", Value: 1 (indicates filled)
    # Add floor at y=0
    for (x = 0; x < WIDTH; x++) {
        chamber[x "," 0] = 1
    }
    highest_y = 0 # Track the highest point reached by any settled rock
    jet_index = 0 # Track current position in the jet pattern (0-based)

    # Main simulation loop for each rock
    for (rock_num = 0; rock_num < TOTAL_ROCKS; rock_num++) {
        # Determine current rock shape and its number of points
        shape_idx = rock_num % 5
        num_points = shape_lengths[shape_idx]

        # Calculate starting position for the new rock
        # Left edge at x=2, bottom edge at y = highest_y + 4
        start_x = 2
        start_y = highest_y + 4

        # Initialize current rock's coordinates in the `rock` array
        for (i = 1; i <= num_points; i++) {
            rock[i, "x"] = start_x + shapes[shape_idx, i, "x"]
            rock[i, "y"] = start_y + shapes[shape_idx, i, "y"]
        }

        # Simulate the falling of the current rock
        while (1) {
            # 1. Apply jet push
            jet_dir = substr(jet_pattern, (jet_index % jet_len) + 1, 1) # AWK substr is 1-based
            jet_index++

            push_dx = 0
            if (jet_dir == ">") {
                push_dx = 1
            } else if (jet_dir == "<") {
                push_dx = -1
            }

            # Check if push is possible and apply if so
            if (push_dx != 0 && can_move(push_dx, 0)) {
                move_rock(push_dx, 0)
            }

            # 2. Apply gravity (move down)
            if (can_move(0, -1)) {
                move_rock(0, -1)
            } else {
                # Rock comes to rest
                # Add rock's points to the chamber and update highest_y
                for (i = 1; i <= num_points; i++) {
                    rx = rock[i, "x"]
                    ry = rock[i, "y"]
                    chamber[rx "," ry] = 1
                    if (ry > highest_y) {
                        highest_y = ry
                    }
                }
                break # Exit while loop, move to next rock
            }
        }
    }

    # Print the final height after all rocks have settled
    print highest_y

    # Exit cleanly (optional, BEGIN block implicitly exits)
    exit 0
}

# No other blocks (like main processing or END) are needed as
# all logic is self-contained within the BEGIN block for this problem.
