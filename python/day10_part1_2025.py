
import re
from pathlib import Path

def bits(x: int) -> int:
    return x.bit_count()

def min_weight(mat, r, c):
    a = [row[:] for row in mat]
    col_pivot = [False] * c
    pr = 0
    for col in range(c):
        sel = -1
        for row in range(pr, r):
            if a[row][col]:
                sel = row
                break
        if sel == -1:
            continue
        a[pr], a[sel] = a[sel], a[pr]
        for row in range(r):
            if row != pr and a[row][col]:
                for k in range(col, c + 1):
                    a[row][k] ^= a[pr][k]
        col_pivot[col] = True
        pr += 1
    for row in range(pr, r):
        if a[row][c]:
            return -1
    free = [i for i, p in enumerate(col_pivot) if not p]
    nf = len(free)
    best = 10 ** 9
    limit = 1 << nf
    for mask in range(limit):
        x = [0] * c
        cw = bits(mask)
        for j, col in enumerate(free):
            if (mask >> j) & 1:
                x[col] = 1
        cur = 0
        for col in range(c):
            if col_pivot[col]:
                v = a[cur][c]
                for k in range(col + 1, c):
                    if a[cur][k]:
                        v ^= x[k]
                x[col] = v
                cw += v
                cur += 1
        if cw < best:
            best = cw
    return best

def solve():
    total = 0
    for line in Path("input.txt").read_text().splitlines():
        l = line.find('[')
        r = line.find(']', l)
        if l == -1 or r == -1:
            continue
        target_str = line[l + 1 : r]
        rows = len(target_str)
        target = [1 if ch == '#' else 0 for ch in target_str]

        buttons = []
        for m in re.finditer(r'\(([^)]*)\)', line[r + 1 :]):
            content = m.group(1)
            if not content.strip():
                buttons.append([])
                continue
            buttons.append([int(t) for t in content.split(',')])

        cols = len(buttons)
        matrix = [[0] * (cols + 1) for _ in range(rows)]
        for i in range(rows):
            for j, btn in enumerate(buttons):
                matrix[i][j] = 1 if i in btn else 0
            matrix[i][cols] = target[i]

        mw = min_weight(matrix, rows, cols)
        if mw != -1:
            total += mw
    print(total)

if __name__ == "__main__":
    solve()
