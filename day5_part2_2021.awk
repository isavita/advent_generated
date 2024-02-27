
function abs(x) {
	return x < 0 ? -x : x
}

function sign(x) {
	return x > 0 ? 1 : x < 0 ? -1 : 0
}

BEGIN {
    while ((getline < "input.txt") > 0) {
        split($0, parts, " -> ")
        split(parts[1], start, ",")
        split(parts[2], end, ",")

        x1 = start[1]
        y1 = start[2]
        x2 = end[1]
        y2 = end[2]

        xStep = sign(x2 - x1)
        yStep = sign(y2 - y1)
        steps = abs(x2 - x1) + 1
        if (abs(y2 - y1) > abs(x2 - x1)) {
            steps = abs(y2 - y1) + 1
        }

        for (i = 0; i < steps; i++) {
            pointX = x1 + i * xStep
            pointY = y1 + i * yStep
            overlaps[pointX, pointY]++
        }
    }

    count = 0
    for (point in overlaps) {
        if (overlaps[point] > 1) {
            count++
        }
    }

    print count
}
