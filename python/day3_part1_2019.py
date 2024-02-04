with open('input.txt') as f:
    wires = f.readlines()

w1 = wires[0].strip().split(',')
w2 = wires[1].strip().split(',')

def get_points(wire):
    x, y = 0, 0
    points = set()
    for move in wire:
        direction = move[0]
        distance = int(move[1:])
        for _ in range(distance):
            if direction == 'R':
                x += 1
            elif direction == 'L':
                x -= 1
            elif direction == 'U':
                y += 1
            elif direction == 'D':
                y -= 1
            points.add((x, y))
    return points

w1_points = get_points(w1)
w2_points = get_points(w2)

intersections = w1_points.intersection(w2_points)
intersections.discard((0,0))

min_distance = min(abs(x) + abs(y) for (x,y) in intersections)
print(min_distance)