
import math
import os
import re
import sys

def main():
    n = cleaning_robot(read_file("input.txt"))
    print(n)

def cleaning_robot(input):
    grid = [list(l) for l in input.split("\n")]
    graph = None
    for r, row in enumerate(grid):
        for c, cell in enumerate(row):
            if re.match("[0-9]", cell):
                poi = cell
                distances_from_poi = bfs_get_edge_weights(grid, [r, c])

                if graph is None:
                    for _ in range(len(distances_from_poi)):
                        graph = [[] for _ in range(len(distances_from_poi))]
                
                index = int(poi)
                graph[index] = distances_from_poi

    return dfs(graph, 0, {0: True}, False)

class BfsNode:
    def __init__(self, row, col, distance):
        self.row = row
        self.col = col
        self.distance = distance

def bfs_get_edge_weights(grid, start):
    poi_to_distance = {grid[start[0]][start[1]]: 0}
    queue = [BfsNode(start[0], start[1], 0)]
    visited = {}
    
    while queue:
        front = queue.pop(0)
        
        if (front.row, front.col) in visited:
            continue
        visited[(front.row, front.col)] = True

        if re.match("[0-9]", grid[front.row][front.col]):
            poi_to_distance[grid[front.row][front.col]] = front.distance

        for d in [(0, -1), (0, 1), (1, 0), (-1, 0)]:
            next_row, next_col = front.row + d[0], front.col + d[1]

            if grid[next_row][next_col] != "#":
                queue.append(BfsNode(next_row, next_col, front.distance + 1))

    distances = [0] * len(poi_to_distance)
    for num_str, dist in poi_to_distance.items():
        n = int(num_str)
        distances[n] = dist

    return distances

def dfs(graph, entry_index, visited, return_to_zero):
    if len(graph) == len(visited):
        if return_to_zero:
            return graph[entry_index][0]
        return 0
    
    min_distance = math.inf
    for i, val in enumerate(graph[entry_index]):
        if i not in visited:
            visited[i] = True

            dist = val + dfs(graph, i, visited, return_to_zero)
            min_distance = min(min_distance, dist)

            del visited[i]

    return min_distance

def read_file(path_from_caller):
    current_dir = os.path.dirname(os.path.abspath(__file__))
    file_path = os.path.join(current_dir, path_from_caller)

    with open(file_path, 'r') as file:
        content = file.read()

    return content.rstrip("\n")

if __name__ == "__main__":
    main()
