
BEGIN {
    while ((getline line < "input.txt") > 0) {
        if (line ~ /^[0-9]/) {
            split(line, a, ",")
            x[a[1]] = 1
            y[a[2]] = 1
            points[a[1], a[2]] = 1
        } else if (line ~ /^fold/) {
            split(line, a, "=")
            axis = substr(a[1], length(a[1]), 1)
            folds[axis] = a[2]
            foldCount++
            if (foldCount == 1) break
        }
    }
    axis = (folds["x"] != "") ? "x" : "y"
    value = folds[axis]
    for (point in points) {
        split(point, p, SUBSEP)
        x_val = p[1]
        y_val = p[2]
        if (axis == "x" && x_val > value) {
            new_x = 2 * value - x_val
            new_points[new_x, y_val] = 1
        } else if (axis == "y" && y_val > value) {
            new_y = 2 * value - y_val
            new_points[x_val, new_y] = 1
        } else {
            new_points[x_val, y_val] = 1
        }
    }
    for (point in new_points) {
        count++
    }
    print count
    exit
}
