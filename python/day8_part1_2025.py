import sys, heapq

def find(p, x):
    while p[x] != x:
        p[x] = p[p[x]]
        x = p[x]
    return x

def union(p, sz, a, b):
    ra, rb = find(p, a), find(p, b)
    if ra == rb: return
    if sz[ra] < sz[rb]:
        ra, rb = rb, ra
    p[rb] = ra
    sz[ra] += sz[rb]

def main():
    pts = []
    with open('input.txt') as f:
        for line in f:
            parts = line.split(',')
            if len(parts) == 3:
                pts.append(tuple(map(int, map(str.strip, parts))))
    n = len(pts)
    if n < 2:
        return
    edges = (( (pts[i][0]-pts[j][0])**2 + (pts[i][1]-pts[j][1])**2 + (pts[i][2]-pts[j][2])**2, i, j )
             for i in range(n) for j in range(i+1, n))
    smallest = heapq.nsmallest(1000, edges, key=lambda x: x[0])
    parent = list(range(n))
    size = [1]*n
    for _, u, v in smallest:
        union(parent, size, u, v)
    comps = {}
    for i in range(n):
        r = find(parent, i)
        comps[r] = size[r]
    top = sorted(comps.values(), reverse=True)[:3]
    result = 1
    for s in top:
        result *= s
    print(f"Product of three largest circuit sizes: {result}")

if __name__ == "__main__":
    main()
