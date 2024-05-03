#!/usr/bin/awk -f

BEGIN {
    # Hardcoded path to the input file
    file_path = "input.txt";

    # Open the file
    while ((getline line < file_path) > 0) {
        # Initialize positions of Santa and Robo-Santa
        # Both start at the origin (0, 0)
        sx = sy = rx = ry = 0;

        # Associative array to keep track of houses
        # Starting house gets two presents initially
        houses[sx "," sy] = 2;

        # Read the entire line of directions
        len = length(line);
        for (i = 1; i <= len; i++) {
            # Get the current move
            move = substr(line, i, 1);

            # Determine whose turn it is
            if (i % 2 == 1) {
                # Santa's turn
                if (move == "^") sy++;
                else if (move == "v") sy--;
                else if (move == ">") sx++;
                else if (move == "<") sx--;

                # Record the position
                coord = sx "," sy;
            } else {
                # Robo-Santa's turn
                if (move == "^") ry++;
                else if (move == "v") ry--;
                else if (move == ">") rx++;
                else if (move == "<") rx--;

                # Record the position
                coord = rx "," ry;
            }

            # Ensure the house at the current position is recorded
            houses[coord]++;
        }
    }
    close(file_path);

    # Count unique houses
    for (house in houses) {
        count++;
    }
    print count;
}
