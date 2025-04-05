
#!/bin/bash

main() {
    awk '
    BEGIN { reading_points = 1; num_folds = 0 }

    /^$/ { reading_points = 0; next }

    reading_points && /,/ {
        split($0, coords, ",");
        points[coords[1] "," coords[2]] = 1;
        next;
    }

    !reading_points && /=/ {
        split($0, parts, "=");
        axis = substr(parts[1], length(parts[1]), 1);
        value = parts[2] + 0;
        num_folds++;
        fold_axis[num_folds] = axis;
        fold_value[num_folds] = value;
        next;
    }

    END {
        for (f = 1; f <= num_folds; ++f) {
            current_axis = fold_axis[f];
            current_value = fold_value[f];

            delete new_points;

            for (coord in points) {
                split(coord, xy, ",");
                x = xy[1] + 0;
                y = xy[2] + 0;

                if (current_axis == "x" && x > current_value) {
                    x = 2 * current_value - x;
                } else if (current_axis == "y" && y > current_value) {
                    y = 2 * current_value - y;
                }
                new_points[x "," y] = 1;
            }

            delete points;
            for (coord in new_points) {
                points[coord] = 1;
            }

            if (f == 1) {
                print "Number of dots visible after first fold:", length(points);
            }
        }

        max_x = -1; max_y = -1;
        for (coord in points) {
            split(coord, xy, ",");
            x = xy[1] + 0;
            y = xy[2] + 0;
            if (x > max_x) max_x = x;
            if (y > max_y) max_y = y;
        }

        if (max_x == -1) exit; # No points left

        for (y = 0; y <= max_y; ++y) {
            line = "";
            for (x = 0; x <= max_x; ++x) {
                if ((x "," y) in points) {
                    line = line "#";
                } else {
                    line = line ".";
                }
            }
            print line;
        }
    }
    ' input.txt
}

main
