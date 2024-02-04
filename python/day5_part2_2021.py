
with open("input.txt", "r") as file:
    lines = []
    for line in file:
        parts = line.strip().split(" -> ")
        start = parts[0].split(",")
        end = parts[1].split(",")

        x1, y1 = map(int, start)
        x2, y2 = map(int, end)

        lines.append([x1, y1, x2, y2])

overlaps = {}

for line in lines:
    x1, y1, x2, y2 = line

    xStep = 1 if x2 - x1 > 0 else -1 if x2 - x1 < 0 else 0
    yStep = 1 if y2 - y1 > 0 else -1 if y2 - y1 < 0 else 0
    steps = abs(x2 - x1) + 1
    if abs(y2 - y1) > abs(x2 - x1):
        steps = abs(y2 - y1) + 1

    for i in range(steps):
        point = (x1 + i * xStep, y1 + i * yStep)
        overlaps[point] = overlaps.get(point, 0) + 1

count = sum(1 for v in overlaps.values() if v > 1)
print(count)
