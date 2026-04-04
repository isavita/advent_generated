
#!/usr/bin/env bash

input=${1:-input.txt}

python3 - <<'PY' "$input"
import math, sys

path = sys.argv[1]
grid = [line.rstrip('\n') for line in open(path)]
asts = [(x, y) for y, row in enumerate(grid) for x, c in enumerate(row) if c == '#']

def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

best = None
best_seen = -1

for i, (x1, y1) in enumerate(asts):
    seen = set()
    for j, (x2, y2) in enumerate(asts):
        if i == j:
            continue
        dx, dy = x2 - x1, y2 - y1
        g = gcd(abs(dx), abs(dy))
        seen.add((dx // g, dy // g))
    if len(seen) > best_seen:
        best_seen = len(seen)
        best = (x1, y1)

print(best_seen)

sx, sy = best
targets = []
for x, y in asts:
    if (x, y) == best:
        continue
    dx, dy = x - sx, y - sy
    angle = math.atan2(dx, -dy)
    if angle < 0:
        angle += 2 * math.pi
    dist = dx * dx + dy * dy
    targets.append((angle, dist, x, y))

targets.sort()

count = 0
used = set()
while True:
    last = -1.0
    for angle, dist, x, y in targets:
        if (x, y) in used:
            continue
        if angle > last + 1e-15:
            used.add((x, y))
            count += 1
            last = angle
            if count == 200:
                print(x * 100 + y)
                sys.exit(0)
PY
