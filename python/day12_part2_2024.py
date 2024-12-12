
import sys

def solve():
    with open("input.txt") as f:
        lines = f.readlines()

    graph = [list(line.strip()) for line in lines if line.strip()]
    H = len(graph)
    W = len(graph[0])

    moves = [(-1, 0), (0, -1), (1, 0), (0, 1)]
    
    total_sum = 0

    for y in range(H):
        for x in range(W):
            if graph[y][x] == '.':
                continue

            area = 0
            target = graph[y][x]
            visited = set()
            side = {'left': set(), 'up': set(), 'right': set(), 'down': set()}

            q = [(x, y, "")]
            while q:
                cx, cy, label = q.pop(0)

                if graph[cy][cx] != target:
                    if label and (cx,cy) not in visited:
                        add_outer(label, side, cx, cy)
                    continue

                visited.add((cx,cy))
                area += 1
                graph[cy][cx] = '.'

                for dx, dy in moves:
                    nx, ny = cx + dx, cy + dy
                    if 0 <= nx < W and 0 <= ny < H:
                        q.append((nx, ny, get_label(dx,dy)))
                    else:
                        add_outer(get_label(dx,dy), side, nx, ny)

            outer = count_outer(side)
            total_sum += area * outer

    print(total_sum)

def add_outer(label, side, x, y):
    key = (y,x) if label in ["up", "down"] else (x,y)
    side[label].add(key)

def count_outer(side):
    outer = 0
    for label, keys in side.items():
        sorted_keys = sorted(list(keys))
        temp = []
        for key in sorted_keys:
            if not check(temp, key):
                outer += 1
            temp.append(key)
    return outer

def check(ary, key):
    i, j = key
    for di, dj in [(0,-1), (0,1)]:
        neighbor = (i+di, j+dj)
        if neighbor in ary:
            return True
    return False

def get_label(dx, dy):
    if dx == -1: return "left"
    if dx == 1: return "right"
    if dy == -1: return "up"
    return "down"

solve()
