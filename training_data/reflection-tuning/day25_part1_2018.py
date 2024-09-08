class DisjointSet:
    def __init__(self, n):
        self.parent = list(range(n))
        self.rank = [0] * n

    def find(self, item):
        if self.parent[item] != item:
            self.parent[item] = self.find(self.parent[item])
        return self.parent[item]

    def union(self, x, y):
        xroot = self.find(x)
        yroot = self.find(y)
        if xroot == yroot:
            return
        if self.rank[xroot] < self.rank[yroot]:
            self.parent[xroot] = yroot
        elif self.rank[xroot] > self.rank[yroot]:
            self.parent[yroot] = xroot
        else:
            self.parent[yroot] = xroot
            self.rank[xroot] += 1

def manhattan_distance(p1, p2):
    return sum(abs(a - b) for a, b in zip(p1, p2))

def count_constellations(points):
    n = len(points)
    ds = DisjointSet(n)
    
    for i in range(n):
        for j in range(i+1, n):
            if manhattan_distance(points[i], points[j]) <= 3:
                ds.union(i, j)
    
    return len(set(ds.find(i) for i in range(n)))

# Read input
with open('input.txt', 'r') as f:
    points = [tuple(map(int, line.strip().split(','))) for line in f]

# Solve and print result
result = count_constellations(points)
print(result)
