
#!/usr/bin/awk -f

# Main logic encapsulated in END block, acting like a main entry point
END {
    # Read input file into grid array
    rows = 0
    cols = 0
    while ((getline line < "input.txt") > 0) {
        rows++
        current_cols = length(line)
        if (cols == 0) {
            cols = current_cols
        } else if (cols != current_cols) {
             # Handle potentially inconsistent line lengths if needed, though problem implies uniform grid
             # For simplicity, we assume uniform grid based on first line
        }
        for (j = 1; j <= cols; j++) {
            grid[rows, j] = substr(line, j, 1)
        }
    }
    close("input.txt")

    # Simulation loop
    while (1) {
        changes = 0

        # Create the next state grid by copying current grid
        # Necessary to base decisions on the state at the start of the iteration
        delete new_grid
        for (i = 1; i <= rows; i++) {
           for (j = 1; j <= cols; j++) {
                new_grid[i, j] = grid[i, j]
           }
        }

        # Iterate through each cell
        for (i = 1; i <= rows; i++) {
            for (j = 1; j <= cols; j++) {
                current_state = grid[i, j]
                if (current_state == ".") continue # Floor never changes

                occupied_neighbors = 0
                # Count occupied neighbors in the *current* grid
                for (dx = -1; dx <= 1; dx++) {
                    for (dy = -1; dy <= 1; dy++) {
                        if (dx == 0 && dy == 0) continue # Skip self
                        nx = i + dx
                        ny = j + dy
                        # Check bounds
                        if (nx >= 1 && nx <= rows && ny >= 1 && ny <= cols) {
                            if (grid[nx, ny] == "#") {
                                occupied_neighbors++
                            }
                        }
                    }
                }

                # Apply rules and update the *new_grid*
                if (current_state == "L" && occupied_neighbors == 0) {
                    new_grid[i, j] = "#"
                    changes++
                } else if (current_state == "#" && occupied_neighbors >= 4) {
                    new_grid[i, j] = "L"
                    changes++
                }
            }
        }

        # If no changes occurred, the simulation is stable
        if (changes == 0) break

        # Update the main grid for the next iteration
         for (i = 1; i <= rows; i++) {
           for (j = 1; j <= cols; j++) {
                grid[i, j] = new_grid[i, j]
           }
        }
    }

    # Count the occupied seats in the final stable grid
    total_occupied = 0
    for (i = 1; i <= rows; i++) {
        for (j = 1; j <= cols; j++) {
            if (grid[i, j] == "#") {
                total_occupied++
            }
        }
    }
    print total_occupied
}

