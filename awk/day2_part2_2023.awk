#!/usr/bin/awk -f

BEGIN {
    # Hardcoded path to the input file
    file_path = "input.txt";
    total_power = 0;

    # Open the file
    while ((getline line < file_path) > 0) {
        # Reset maximum seen for each color in this game
        max_seen_red = max_seen_green = max_seen_blue = 0;

        # Extract game ID
        split(line, parts, ":");
        game_id = substr(parts[1], 6);  # Extract number from something like "Game 11"

        # Split the segments that show subsets of cubes
        n = split(parts[2], sets, ";");
        for (i = 1; i <= n; i++) {
            # Reset counts for this subset
            count_red = count_green = count_blue = 0;

            # Count cubes of each color
            m = split(sets[i], cubes, ",");
            for (j = 1; j <= m; j++) {
                if (cubes[j] ~ /red/) {
                    split(cubes[j], tmp, " ");
                    count_red = tmp[1];
                } else if (cubes[j] ~ /green/) {
                    split(cubes[j], tmp, " ");
                    count_green = tmp[1];
                } else if (cubes[j] ~ /blue/) {
                    split(cubes[j], tmp, " ");
                    count_blue = tmp[1];
                }

                # Update maximum seen for this game
                if (count_red > max_seen_red) max_seen_red = count_red;
                if (count_green > max_seen_green) max_seen_green = count_green;
                if (count_blue > max_seen_blue) max_seen_blue = count_blue;
            }
        }

        # Calculate the power of the minimum set of cubes for this game
        game_power = max_seen_red * max_seen_green * max_seen_blue;
        total_power += game_power;
    }

    # Print the total sum of the powers
    print total_power;
    close(file_path);
}
