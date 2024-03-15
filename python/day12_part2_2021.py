from collections import defaultdict

def find_paths(graph, start, end, visited, small_twice=False):
    if start == end:
        return 1
    if start.islower() and start in visited:
        if small_twice:
            return 0
        small_twice = True
    visited.add(start)
    paths = 0
    for neighbor in graph[start]:
        paths += find_paths(graph, neighbor, end, visited.copy(), small_twice)
    return paths

with open("input.txt", "r") as f:
    lines = f.readlines()

graph = defaultdict(list)
for line in lines:
    a, b = line.strip().split("-")
    if a != "end" and b != "start":
        graph[a].append(b)
    if b != "end" and a != "start":
        graph[b].append(a)

print(find_paths(graph, "start", "end", set()))
print(find_paths(graph, "start", "end", set(), True))