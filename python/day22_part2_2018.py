
from heapq import heappush, heappop

geologicY = 16807
geologicX = 48271
caveModulo = 20183

TypeRocky = 0
TypeWet = 1
TypeNarrow = 2

ToolNone = 1
ToolTorch = 2
ToolGear = 4

class Map:
    def __init__(self, input_str):
        self.geologic_indices_cache = {}
        self.erosion_levels_cache = {}
        parts = input_str.split("\n")
        self.depth = int(parts[0].split(": ")[1])
        self.target = tuple(map(int, parts[1].split(": ")[1].split(",")))

    def geologic_index(self, x, y):
        if y in self.geologic_indices_cache and x in self.geologic_indices_cache[y]:
            return self.geologic_indices_cache[y][x]
        if x == 0 and y == 0:
            index = 0
        elif x == self.target[0] and y == self.target[1]:
            index = 0
        elif y == 0:
            index = x * geologicY
        elif x == 0:
            index = y * geologicX
        else:
            index = self.erosion_level(x-1, y) * self.erosion_level(x, y-1)
        self.geologic_indices_cache.setdefault(y, {})[x] = index
        return index

    def erosion_level(self, x, y):
        if y in self.erosion_levels_cache and x in self.erosion_levels_cache[y]:
            return self.erosion_levels_cache[y][x]
        level = (self.geologic_index(x, y) + self.depth) % caveModulo
        self.erosion_levels_cache.setdefault(y, {})[x] = level
        return level

    def type(self, x, y):
        return self.erosion_level(x, y) % 3

    def neighbors(self, pos, equip):
        n = []
        for dx, dy in [(1, 0), (0, 1), (-1, 0), (0, -1)]:
            nx, ny = pos[0] + dx, pos[1] + dy
            if nx >= 0 and ny >= 0:
                t = self.type(nx, ny)
                if equip & allowed(t):
                    n.append((nx, ny, equip, 1))
                    n.append((nx, ny, equip ^ allowed(t), 8))
        return n

def allowed(region_type):
    if region_type == TypeRocky:
        return ToolGear | ToolTorch
    elif region_type == TypeWet:
        return ToolGear | ToolNone
    elif region_type == TypeNarrow:
        return ToolTorch | ToolNone
    else:
        raise Exception("Unknown region type: {}".format(region_type))

def rescue(input_str):
    m = Map(input_str)
    target = m.target
    queue = [(0, (0, 0), ToolTorch)]
    distances = {(0, 0, ToolTorch): 0}
    while queue:
        time, pos, equip = heappop(queue)
        if pos == target and equip == ToolTorch:
            return time
        if pos[0] > 8 * target[0] or pos[1] > 8 * target[1]:
            continue
        if time < distances.get((pos, equip), float('inf')):
            for n in m.neighbors(pos, equip):
                new_time = time + n[3]
                if new_time < distances.get((n[0], n[1], n[2]), float('inf')):
                    distances[(n[0], n[1], n[2])] = new_time
                    heappush(queue, (new_time, (n[0], n[1]), n[2]))
    return 0

with open("input.txt", "r") as file:
    input_data = file.read()

print(rescue(input_data))
