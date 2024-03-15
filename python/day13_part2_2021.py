with open("input.txt", "r") as file:
    lines = file.readlines()

points = set()
folds = []
reading_points = True

for line in lines:
    line = line.strip()
    if not line:
        reading_points = False
        continue
    if reading_points:
        x, y = map(int, line.split(","))
        points.add((x, y))
    else:
        axis, value = line.split("=")
        folds.append((axis[-1], int(value)))

for i, fold in enumerate(folds):
    new_points = set()
    for x, y in points:
        if fold[0] == "x" and x > fold[1]:
            new_points.add((2 * fold[1] - x, y))
        elif fold[0] == "y" and y > fold[1]:
            new_points.add((x, 2 * fold[1] - y))
        else:
            new_points.add((x, y))
    points = new_points
    if i == 0:
        print("Number of dots visible after first fold:", len(points))

max_x = max(x for x, y in points)
max_y = max(y for x, y in points)

grid = [["." for _ in range(max_x + 1)] for _ in range(max_y + 1)]
for x, y in points:
    grid[y][x] = "#"

for row in grid:
    print("".join(row))