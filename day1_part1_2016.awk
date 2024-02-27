
#!/usr/bin/awk -f

BEGIN {
    # Starting position facing North
    x = 0
    y = 0
    dirIndex = 0

    # Directions: North, East, South, West
    directions["0,0"] = 0; directions["0,1"] = 1
    directions["1,0"] = 1; directions["1,1"] = 0
    directions["2,0"] = 0; directions["2,1"] = -1
    directions["3,0"] = -1; directions["3,1"] = 0
}

{
    # Split the line into instructions
    n = split($0, instruction, /, */)
    for (i = 1; i <= n; i++) {
        # Get the turn and blocks
        turn = substr(instruction[i], 1, 1)
        blocks = substr(instruction[i], 2)

        # Update the direction index
        if (turn == "R") {
            dirIndex = (dirIndex + 1) % 4
        } else {
            dirIndex = (dirIndex - 1 + 4) % 4
        }

        # Update the position
        x += directions[dirIndex ",0"] * blocks
        y += directions[dirIndex ",1"] * blocks
    }
}

END {
    # Print the final result
    print ((x < 0 ? -x : x) + (y < 0 ? -y : y))
}
