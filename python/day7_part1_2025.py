
import sys

def main() -> None:
    lines = sys.stdin.read().splitlines()
    if not lines:
        return

    h, w = len(lines), len(lines[0])
    grid = [list(row) for row in lines]

    # locate start
    for y in range(h):
        try:
            x = grid[y].index('S')
            sy, sx = y, x
            break
        except ValueError:
            continue
    else:
        return

    splits = 0
    active = [False] * w
    next_row = [False] * w
    active[sx] = True

    for y in range(sy, h):
        # reset next_row
        for i in range(w):
            next_row[i] = False

        row = grid[y]
        for x in range(w):
            if not active[x]:
                continue
            if row[x] == '^':
                splits += 1
                if x:
                    next_row[x - 1] = True
                if x + 1 < w:
                    next_row[x + 1] = True
            else:
                next_row[x] = True

        if not any(next_row):
            break
        active, next_row = next_row, active   # swap buffers

    print(splits)


if __name__ == "__main__":
    # read from file "input.txt" as required
    with open("input.txt", "r") as f:
        sys.stdin = f
        main()
