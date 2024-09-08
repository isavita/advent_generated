from collections import defaultdict
import itertools

def parse_input(input_str):
    graph = defaultdict(set)
    for line in input_str.strip().split('\n'):
        node, connections = line.split(': ')
        for conn in connections.split():
            graph[node].add(conn)
            graph[conn].add(node)
    return graph

def dfs(graph, start, visited):
    stack = [start]
    count = 0
    while stack:
        node = stack.pop()
        if node not in visited:
            visited.add(node)
            count += 1
            stack.extend(graph[node] - visited)
    return count

def find_groups(graph, removed_edges):
    visited = set()
    groups = []
    for node in graph:
        if node not in visited:
            group_size = dfs(graph, node, visited)
            groups.append(group_size)
    return groups

def solve(input_str):
    graph = parse_input(input_str)
    all_edges = set((min(a, b), max(a, b)) for a in graph for b in graph[a])
    
    max_product = 0
    for edges_to_remove in itertools.combinations(all_edges, 3):
        temp_graph = {node: set(connections) for node, connections in graph.items()}
        for a, b in edges_to_remove:
            temp_graph[a].remove(b)
            temp_graph[b].remove(a)
        
        groups = find_groups(temp_graph, edges_to_remove)
        if len(groups) == 2:
            product = groups[0] * groups[1]
            max_product = max(max_product, product)
    
    return max_product

# Example usage:
input_str = """
jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
"""

result = solve(input_str)
print(result)  # This should print 54 for the given example
