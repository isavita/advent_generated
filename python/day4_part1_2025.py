
def main():
    with open("input.txt") as f:
        lines = [line.rstrip("\n") for line in f if line.strip()]
    if not lines:
        return
    rows, cols = len(lines), len(lines[0])
    grid = [list(row) for row in lines]
    dirs = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
    acc = 0
    for y in range(rows):
        for x in range(cols):
            if grid[y][x] != '@':
                continue
            cnt = sum(
                0 <= y + dy < rows and 0 <= x + dx < cols and grid[y + dy][x + dx] == '@'
                for dy, dx in dirs
            )
            if cnt < 4:
                acc += 1
    print(acc)


if __name__ == "__main__":
    main()
