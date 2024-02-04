
with open("input.txt", "r") as file:
    points = {}
    folds = []
    readingPoints = True

    for line in file:
        line = line.strip()
        if line == "":
            readingPoints = False
            continue
        if readingPoints:
            coords = line.split(",")
            x, y = int(coords[0]), int(coords[1])
            points[(x, y)] = True
        else:
            folds.append(line)

    fold = folds[0].split()[2]  # "fold along x=5" -> "x=5"
    axis, value = fold.split("=")
    value = int(value)

    newPoints = {}
    if axis == "x":
        for point in points:
            x, y = point
            if x > value:
                x = 2 * value - x
            newPoints[(x, y)] = True
    else:
        for point in points:
            x, y = point
            if y > value:
                y = 2 * value - y
            newPoints[(x, y)] = True

    print(len(newPoints))
