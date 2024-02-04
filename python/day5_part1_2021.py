with open('input.txt', 'r') as file:
    lines = file.readlines()

points = set()
overlap = set()

for line in lines:
    coords = line.strip().split(' -> ')
    x1, y1 = map(int, coords[0].split(','))
    x2, y2 = map(int, coords[1].split(','))

    if x1 == x2:  # Vertical line
        for y in range(min(y1, y2), max(y1, y2) + 1):
            point = (x1, y)
            if point in points:
                overlap.add(point)
            points.add(point)
    elif y1 == y2:  # Horizontal line
        for x in range(min(x1, x2), max(x1, x2) + 1):
            point = (x, y1)
            if point in points:
                overlap.add(point)
            points.add(point)

print(len(overlap))