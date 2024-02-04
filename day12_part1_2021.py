
class Cave:
    def __init__(self):
        self.connections = {}

    def connect_to(self, name):
        self.connections[name] = True

    def disconnect_from(self, name):
        del self.connections[name]

caves = {}

with open("input.txt", "r") as file:
    for line in file:
        paths = line.strip().split("-")
        from_cave, to_cave = paths[0], paths[1]

        if from_cave not in caves:
            caves[from_cave] = Cave()

        if to_cave not in caves:
            caves[to_cave] = Cave()

        caves[from_cave].connect_to(to_cave)
        caves[to_cave].connect_to(from_cave)

count = 0

def dfs(current, visited):
    global count
    if current == "end":
        count += 1
        return

    for next_cave in caves[current].connections:
        if visited.get(next_cave) and next_cave.lower() == next_cave:
            continue

        visited_copy = visited.copy()
        visited_copy[next_cave] = True
        dfs(next_cave, visited_copy)

dfs("start", {"start": True})
print(count)
