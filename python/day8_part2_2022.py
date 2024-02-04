
from collections import defaultdict

def main():
    grid = defaultdict(int)
    with open("input.txt", "r") as file:
        lines = file.readlines()
        for y, line in enumerate(lines):
            for x, b in enumerate(line.strip()):
                grid[(x, y)] = int(b)

    neighbors4 = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    max_score = 0
    for p in grid:
        score = 1
        for n in neighbors4:
            next_pos, view = p, 0
            while True:
                next_pos = tuple(map(sum, zip(next_pos, n)))
                if next_pos in grid:
                    view += 1
                    if grid[next_pos] >= grid[p]:
                        score *= view
                        break
                else:
                    score *= view
                    break

        if score > max_score:
            max_score = score
    print(max_score)

if __name__ == "__main__":
    main()
