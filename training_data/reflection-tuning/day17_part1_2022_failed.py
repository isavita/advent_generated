def solve():
    with open('input.txt', 'r') as file:
        jets = file.read().strip()

    rocks = [
        [(0,0), (1,0), (2,0), (3,0)],
        [(1,0), (0,1), (1,1), (2,1), (1,2)],
        [(0,0), (1,0), (2,0), (2,1), (2,2)],
        [(0,0), (0,1), (0,2), (0,3)],
        [(0,0), (1,0), (0,1), (1,1)]
    ]

    def simulate_rock(rock, jet_index, height):
        x, y = 2, height + 3
        while True:
            # Apply jet
            dx = 1 if jets[jet_index] == '>' else -1
            jet_index = (jet_index + 1) % len(jets)
            if all(0 <= x + dx + rx < 7 and (x + dx + rx, y + ry) not in chamber for rx, ry in rock):
                x += dx
            # Fall down
            if all((x + rx, y + ry - 1) not in chamber for rx, ry in rock) and y > 0:
                y -= 1
            else:
                break
        for rx, ry in rock:
            chamber.add((x + rx, y + ry))
        return jet_index, max(height, y + max(ry for _, ry in rock) + 1)

    chamber = set((x, 0) for x in range(7))
    height, jet_index = 0, 0

    for i in range(2022):
        rock = rocks[i % len(rocks)]
        jet_index, height = simulate_rock(rock, jet_index, height)

    print(height - 1)  # Subtract 1 because the floor is at y=0

solve()
