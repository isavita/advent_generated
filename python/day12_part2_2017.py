
adj = {}

with open("input.txt", "r") as file:
    for line in file:
        parts = line.strip().split(" <-> ")
        from_node = int(parts[0])
        to_nodes = parts[1].split(", ")

        if from_node not in adj:
            adj[from_node] = []
        
        for to_node in to_nodes:
            to = int(to_node)
            adj[from_node].append(to)

            if to not in adj:
                adj[to] = []
            adj[to].append(from_node)

visited = {}
groups = 0

def DFS(node):
    visited[node] = True
    for neighbor in adj[node]:
        if not visited.get(neighbor, False):
            DFS(neighbor)

for node in adj.keys():
    if not visited.get(node, False):
        DFS(node)
        groups += 1

print(groups)
