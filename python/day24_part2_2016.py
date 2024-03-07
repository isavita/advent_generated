from itertools import permutations
import heapq

def read_map(filename):
    with open(filename) as f:
        return [list(line.strip()) for line in f]

def neighbors(x, y):
    return [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

def find_interests(layout):
    interests = {}
    for y, row in enumerate(layout):
        for x, val in enumerate(row):
            if val.isdigit():
                interests[val] = (x, y)
    return interests

def shortest_path(layout, start, goal):
    queue = [(0, start)]
    visited = set()
    while queue:
        cost, (x, y) = heapq.heappop(queue)
        if (x, y) == goal:
            return cost
        if (x, y) in visited:
            continue
        visited.add((x, y))
        for nx, ny in neighbors(x, y):
            if layout[ny][nx] != '#' and (nx, ny) not in visited:
                heapq.heappush(queue, (cost+1, (nx, ny)))
    return float('inf')

def solve_tsp_with_return(distances):
    nodes = list(distances.keys())
    min_path_length = float('inf')
    for perm in permutations(nodes[1:]):
        path_length = 0
        current = '0'
        for next in perm:
            path_length += distances[current][next]
            current = next
        path_length += distances[current]['0']  # Return to '0'
        min_path_length = min(min_path_length, path_length)
    return min_path_length

def main():
    layout = read_map('input.txt')
    interests = find_interests(layout)
    distances = {i: {} for i in interests}
    
    for i in interests:
        for j in interests:
            if i != j:
                distances[i][j] = shortest_path(layout, interests[i], interests[j])

    answer = solve_tsp_with_return(distances)
    print(answer)

if __name__ == "__main__":
    main()
