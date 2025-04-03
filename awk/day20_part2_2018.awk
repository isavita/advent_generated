
#!/usr/bin/awk -f

BEGIN {
    SUBSEP="," # Separator for coordinate keys

    if ((getline line < "input.txt") <= 0) {
        exit 1
    }
    close("input.txt")

    x = 0
    y = 0
    dist = 0
    rooms[x SUBSEP y] = 0
    sp = 0 # Stack pointer

    # Predefine direction deltas
    dx["N"] = 0; dy["N"] = -1
    dx["E"] = 1; dy["E"] = 0
    dx["S"] = 0; dy["S"] = 1
    dx["W"] = -1; dy["W"] = 0

    # Process characters between ^ and $
    n = split(substr(line, 2, length(line)-2), chars, "")
    for (i = 1; i <= n; ++i) {
        char = chars[i]

        if (char == "(") {
            # Push current position and distance
            stack[sp++] = x SUBSEP y SUBSEP dist
        } else if (char == "|") {
            # Restore position/distance from state at last '('
            split(stack[sp-1], state, SUBSEP)
            x = state[1]
            y = state[2]
            dist = state[3]
        } else if (char == ")") {
            # Pop state completely
            split(stack[--sp], state, SUBSEP)
            x = state[1]
            y = state[2]
            dist = state[3]
        } else { # N, E, S, W movement
            nx = x + dx[char]
            ny = y + dy[char]
            # Distance increases along the current path segment
            current_path_dist = dist + 1
            key = nx SUBSEP ny

            # Update room's minimum distance if this path is shorter or first visit
            if (!(key in rooms) || current_path_dist < rooms[key]) {
                rooms[key] = current_path_dist
            }

            # Update current position and the distance for the *current path*
            x = nx
            y = ny
            dist = current_path_dist
        }
    }
}

END {
    max_d = 0
    count_1000 = 0
    for (key in rooms) {
        d = rooms[key]
        if (d > max_d) {
            max_d = d
        }
        if (d >= 1000) {
            count_1000++
        }
    }
    print max_d
    print count_1000
}
