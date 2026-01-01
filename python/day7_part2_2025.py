import sys

def main():
    lines = [l.rstrip('\n') for l in open('input.txt') if l.strip()]
    if not lines:
        print(0)
        return
    h, w = len(lines), len(lines[0])
    grid = [list(l) for l in lines]
    for y in range(h):
        for x in range(w):
            if grid[y][x] == 'S':
                sx, sy = x, y
                break
        else:
            continue
        break
    else:
        print(0)
        return
    counts = {sx: 1}
    for y in range(sy, h):
        nxt = {}
        row = grid[y]
        for x, cnt in counts.items():
            if 0 <= x < w and row[x] == '^':
                for nx in (x - 1, x + 1):
                    nxt[nx] = nxt.get(nx, 0) + cnt
            else:
                nxt[x] = nxt.get(x, 0) + cnt
        counts = nxt
    print(sum(counts.values()))

if __name__ == "__main__":
    main()
