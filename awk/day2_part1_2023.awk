#!/usr/bin/awk -f

BEGIN {
    # Hardcoded path to the input file
    file_path = "input.txt";

    # Max number of each cube color
    max_red = 12;
    max_green = 13;
    max_blue = 14;

    # Open the file
    while ((getline line < file_path) > 0) {
        # Reset maximums seen in each game
        game_possible = 1;
        max_seen_red = max_seen_green = max_seen_blue = 0;

        # Extract game ID
        split(line, parts, ":");
        game_id = substr(parts[1], 6);  # Extract number from something like "Game 11"

        # Split the segments that show subsets of cubes
        n = split(parts[2], sets, ";");
        for (i = 1; i <= n && game_possible; i++) {
            # Reset counts for this subset
            count_red = count_green = count_blue = 0;

            # Count cubes of each color
            m = split(sets[i], cubes, ",");
            for (j = 1; j <= m; j++) {
                if (cubes[j] ~ /red/) {
                    split(cubes[j], tmp, " ");
                    count_red += tmp[1];
                } else if (cubes[j] ~ /green/) {
                    split(cubes[j], tmp, " ");
                    count_green += tmp[1];
                } else if (cubes[j] ~ /blue/) {
                    split(cubes[j], tmp, " ");
                    count_blue += tmp[1];
                }
            }

            # Determine if this subset makes the game impossible
            if (count_red > max_red || count_green > max_green || count_blue > max_blue) {
                game_possible = 0;
            }
        }

        # If game is possible, add its ID to the total
        if (game_possible) {
            sum_ids += game_id;
        }
    }

    # Print the sum of the IDs of possible games
    print sum_ids;
    close(file_path);
}
