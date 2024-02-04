
class Point:
    def __init__(self, x, y, z, t):
        self.x = x
        self.y = y
        self.z = z
        self.t = t

def abs(x):
    if x < 0:
        return -x
    return x

def manhattan_distance(a, b):
    return abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z) + abs(a.t - b.t)

class UnionFind:
    def __init__(self, size):
        self.parent = [i for i in range(size)]

    def find(self, x):
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])
        return self.parent[x]

    def union(self, x, y):
        root_x = self.find(x)
        root_y = self.find(y)
        if root_x != root_y:
            self.parent[root_x] = root_y

points = []
with open("input.txt", "r") as file:
    for line in file:
        coords = line.strip().split(",")
        x, y, z, t = map(int, coords)
        points.append(Point(x, y, z, t))

uf = UnionFind(len(points))
for i in range(len(points)):
    for j in range(len(points)):
        if manhattan_distance(points[i], points[j]) <= 3:
            uf.union(i, j)

constellation_count = sum(1 for i, parent in enumerate(uf.parent) if i == parent)
print(constellation_count)
