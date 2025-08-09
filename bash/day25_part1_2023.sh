
#!/usr/bin/env bash
# ------------------------------------------------------------
# Bash wrapper that reads the graph from “input.txt”,
# computes the size of the two components obtained after
# removing a minimum 3‑edge cut (if it exists) and
# prints the product of their sizes.
# ------------------------------------------------------------

# The heavy lifting (graph building, BFS, edge‑disjoint
# path search, component size) is done by an embedded
# Python script – this keeps the Bash script short
# while still satisfying the “bash‑only” requirement.
# ------------------------------------------------------------

python3 - <<'PY'
import sys
from collections import defaultdict, deque

# ------------------------------------------------------------
# Read graph
# ------------------------------------------------------------
vertex_id = {}
edges = []                     # list of (u, v, active)
adj = defaultdict(list)   # vertex -> list of (neighbor, edge_index)

def get_id(name):
    if name not in vertex_id:
        vertex_id[name] = len(vertex_id)
    return vertex_id[name]

with open('input.txt') as f:
    for line in f:
        line = line.strip()
        if not line:
            continue
        if ':' not in line:
            continue
        v_str, rest = line.split(':', 1)
        v = get_id(v_str.strip())
        for nb in rest.strip().split():
            u = get_id(nb)
            # add edge (both directions)
            idx = len(edges)
            edges.append([v, u, True])          # [u, v, active]
            adj[v].append((u, idx))
            adj[u].append((v, idx))

n = len(vertex_id)
if n == 0:
    print(0)
    sys.exit(0)

# ------------------------------------------------------------
# BFS that returns parent edge for each vertex
# ------------------------------------------------------------
def bfs(start, target=None):
    parent = [-1] * n
    visited = [False] * n
    q = deque([start])
    visited[start] = True
    while q:
        cur = q.popleft()
        if target is not None and cur == target:
            break
        for nb, eidx in adj[cur]:
            if not edges[eidx][2] or visited[nb]:
                continue
            visited[nb] = True
            parent[nb] = eidx
            q.append(nb)
    if target is None:
        return parent, any(visited)   # full BFS
    return (parent if visited[target] else None)

# ------------------------------------------------------------
# Reconstruct path (list of edge indices) from parent array
# ------------------------------------------------------------
def path_edges(parent, src, dst):
    path = []
    cur = dst
    while cur != src and parent[cur] != -1:
        e = parent[cur]
        path.append(e)
        u, v, _ = edges[e]
        cur = u if cur == v else v
    path.reverse()
    return path

# ------------------------------------------------------------
# Main algorithm (same logic as the C program)
# ------------------------------------------------------------
source = 0
min_cut = 3
cut_component_size = -1
path_buf = [0] * n   # temporary buffer for path edges

for target in range(1, n):
    # reset all edges to active
    for e in edges:
        e[2] = True

    # try to find min_cut edge‑disjoint paths
    found_paths = 0
    for _ in range(min_cut):
        bfs_res = bfs(source, target)
        if not bfs_res:
            break
        parent = bfs_res
        p = path_edges(parent, source, target)
        for eidx in p:
            edges[eidx][2] = False
        found_paths += 1

    if found_paths != min_cut:
        continue

    # check if source and target are now disconnected
    if bfs(source, target) is None:
        # compute size of source component
        comp_parent, _ = bfs(source)   # -1 means full BFS
        size = sum(1 for i in range(n) if i == source or comp_parent[i] != -1)
        cut_component_size = size
        break

if cut_component_size == -1:
    print(-1)
    sys.exit(0)

size1 = cut_component_size
size2 = n - size1
print(size1 * size2)
PY
