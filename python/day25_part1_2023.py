from collections import defaultdict

class Vertex(str):
    pass

class Edge:
    def __init__(self, start, end, weight):
        self.start = start
        self.end = end
        self.weight = weight

def parse_input(input_lines):
    graph = defaultdict(dict)
    for line in input_lines:
        parts = line.split(": ")
        vertex = Vertex(parts[0])
        others = parts[1].split(" ")
        for other in others:
            other_vertex = Vertex(other)
            graph[vertex][Edge(vertex, other_vertex, 1)] = None
            graph[other_vertex][Edge(other_vertex, vertex, 1)] = None
    return graph

def breadth_first_search(graph, start, goal_func):
    frontier = [start]
    reached = {start}
    came_from = {start: start}
    while frontier:
        current = frontier.pop(0)
        if goal_func(current):
            return True, came_from
        for next_edge in graph[current].keys():
            if next_edge.end not in reached:
                frontier.append(next_edge.end)
                reached.add(next_edge.end)
                came_from[next_edge.end] = current
    return False, came_from

def reconstruct_path(start, end, came_from):
    path = [end]
    current = end
    while current != start:
        current = came_from[current]
        path.insert(0, current)
    return path

def copy_graph(graph):
    new_graph = defaultdict(dict)
    for vertex, edges in graph.items():
        new_graph[vertex] = dict(edges)
    return new_graph

def solve(input_lines):
    min_cut = 3
    graph = parse_input(input_lines)
    source = next(iter(graph))
    for end in graph:
        if end == source:
            continue
        new_graph = copy_graph(graph)
        for _ in range(min_cut):
            _, came_from = breadth_first_search(new_graph, source, lambda v: v == end)
            path = reconstruct_path(source, end, came_from)
            for i in range(len(path) - 1):
                edge = next(e for e in new_graph[path[i]] if e.end == path[i+1])
                del new_graph[path[i]][edge]
        is_valid, _ = breadth_first_search(new_graph, source, lambda v: v == end)
        if not is_valid:
            separate_graph = new_graph
            break
    _, came_from = breadth_first_search(separate_graph, source, lambda v: False)
    length1 = len(came_from)
    length2 = len(separate_graph) - length1
    return length1 * length2

def read_file(file_name):
    with open(file_name, 'r') as file:
        return [line.strip() for line in file.readlines()]

if __name__ == '__main__':
    input_lines = read_file('input.txt')
    print(solve(input_lines))