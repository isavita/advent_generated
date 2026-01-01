
import sys
from collections import deque

def main() -> None:
    lines = open("input.txt").read().splitlines()
    pts, xs, ys = [], [], []
    for line in lines:
        parts = [p.strip() for p in line.split(',')]
        if len(parts) != 2:
            continue
        try:
            x = int(parts[0])
            y = int(parts[1])
        except ValueError:
            continue
        pts.append((x, y))
        xs.append(x)
        ys.append(y)

    if not pts:
        print("Largest valid area: 0")
        return

    ux = sorted(set(xs))
    uy = sorted(set(ys))
    x_idx = {v: i for i, v in enumerate(ux)}
    y_idx = {v: i for i, v in enumerate(uy)}

    W = 2 * len(ux) + 1
    H = 2 * len(uy) + 1

    colW = [0] * W
    rowH = [0] * H
    colW[0] = 1
    for i, x in enumerate(ux):
        colW[2 * i + 1] = 1
        gap = ux[i + 1] - x - 1 if i + 1 < len(ux) else 0
        colW[2 * i + 2] = max(gap, 0)
    rowH[0] = 1
    for i, y in enumerate(uy):
        rowH[2 * i + 1] = 1
        gap = uy[i + 1] - y - 1 if i + 1 < len(uy) else 0
        rowH[2 * i + 2] = max(gap, 0)

    grid = [bytearray(W) for _ in range(H)]

    n = len(pts)
    for i in range(n):
        ax, ay = pts[i]
        bx, by = pts[(i + 1) % n]
        gx1 = 2 * x_idx[ax] + 1
        gy1 = 2 * y_idx[ay] + 1
        gx2 = 2 * x_idx[bx] + 1
        gy2 = 2 * y_idx[by] + 1
        if gx1 == gx2:
            y0, y1 = (gy1, gy2) if gy1 <= gy2 else (gy2, gy1)
            for y in range(y0, y1 + 1):
                if rowH[y]:
                    grid[y][gx1] = 1
        else:
            x0, x1 = (gx1, gx2) if gx1 <= gx2 else (gx2, gx1)
            for x in range(x0, x1 + 1):
                if colW[x]:
                    grid[gy1][x] = 1

    dq = deque([(0, 0)])
    grid[0][0] = 2
    dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    while dq:
        cx, cy = dq.popleft()
        for dx, dy in dirs:
            nx, ny = cx + dx, cy + dy
            if 0 <= nx < W and 0 <= ny < H and grid[ny][nx] == 0:
                grid[ny][nx] = 2
                dq.append((nx, ny))

    P = [[0] * W for _ in range(H)]
    for y in range(H):
        row_sum = 0
        for x in range(W):
            v = colW[x] * rowH[y] if grid[y][x] != 2 else 0
            row_sum += v
            above = P[y - 1][x] if y else 0
            P[y][x] = row_sum + above

    max_area = 0
    for i in range(n):
        for j in range(i, n):
            ax, ay = pts[i]
            bx, by = pts[j]
            w = abs(ax - bx) + 1
            h = abs(ay - by) + 1
            area = w * h
            if area <= max_area:
                continue
            gx1 = 2 * x_idx[ax] + 1
            gy1 = 2 * y_idx[ay] + 1
            gx2 = 2 * x_idx[bx] + 1
            gy2 = 2 * y_idx[by] + 1
            if gx1 > gx2:
                gx1, gx2 = gx2, gx1
            if gy1 > gy2:
                gy1, gy2 = gy2, gy1
            total = P[gy2][gx2]
            left = P[gy2][gx1 - 1] if gx1 > 0 else 0
            up = P[gy1 - 1][gx2] if gy1 > 0 else 0
            diag = P[gy1 - 1][gx1 - 1] if gx1 > 0 and gy1 > 0 else 0
            valid = total - left - up + diag
            if valid == area:
                max_area = area

    print(f"Largest valid area: {max_area}")

if __name__ == "__main__":
    main()
