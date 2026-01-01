import sys

def main():
    raw = sys.stdin.read().splitlines() if not open('input.txt').readable() else open('input.txt').read().splitlines()
    if not raw:
        print("Total rolls removed: 0")
        return
    r = len(raw)
    c = max(map(len, raw))
    grid = [list(line.ljust(c, '.')) for line in raw]
    dr = (-1, -1, -1, 0, 0, 1, 1, 1)
    dc = (-1, 0, 1, -1, 1, -1, 0, 1)
    removed = 0
    while True:
        to_remove = []
        for i in range(r):
            row = grid[i]
            for j in range(c):
                if row[j] != '@':
                    continue
                cnt = 0
                for k in range(8):
                    ni, nj = i + dr[k], j + dc[k]
                    if 0 <= ni < r and 0 <= nj < c and grid[ni][nj] == '@':
                        cnt += 1
                if cnt < 4:
                    to_remove.append((i, j))
        if not to_remove:
            break
        for i, j in to_remove:
            grid[i][j] = '.'
        removed += len(to_remove)
    print(f"Total rolls removed: {removed}")

if __name__ == "__main__":
    main()
