def solve():
    with open('input.txt', 'r') as f:
        grid = f.read().strip().split('\n')

    infected = {complex(x, y) for y, row in enumerate(grid) for x, cell in enumerate(row) if cell == '#'}
    center = complex(len(grid[0]) // 2, len(grid) // 2)
    pos = center
    direction = -1j  # Up
    infections = 0

    def turn_right(d):
        return d * 1j

    def turn_left(d):
        return d * -1j

    for _ in range(10000):
        if pos in infected:
            direction = turn_right(direction)
            infected.remove(pos)
        else:
            direction = turn_left(direction)
            infected.add(pos)
            infections += 1
        pos += direction

    print(infections)

solve()
