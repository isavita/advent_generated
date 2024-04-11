import heapq

class Coord:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y
    
    def __hash__(self):
        return hash((self.x, self.y))
    
    def __lt__(self, other):
        return (self.x, self.y) < (other.x, other.y)
    
    def add(self, other):
        return Coord(self.x + other.x, self.y + other.y)
    
    def subtract(self, other):
        return Coord(self.x - other.x, self.y - other.y)
    
    def opposite(self):
        return Coord(-self.x, -self.y)

class Grid:
    def __init__(self, input):
        self.width = len(input[0])
        self.height = len(input)
        self.data = {}
        for y, line in enumerate(input):
            for x, char in enumerate(line):
                self.data[Coord(x, y)] = int(char)
    
    def neighbors4(self, coord):
        directions = [Coord(0, -1), Coord(-1, 0), Coord(0, 1), Coord(1, 0)]
        neighbors = []
        for direction in directions:
            neighbor = coord.add(direction)
            if 0 <= neighbor.x < self.width and 0 <= neighbor.y < self.height:
                neighbors.append(neighbor)
        return neighbors

    def a_star_constrained(self, start, goal, min_straight, max_straight):
        start_info = (start, Coord(0, 0), 0)  # (coord, direction, num_straight)
        frontier = []
        heapq.heappush(frontier, (0, start_info))
        came_from = {start_info: None}
        cost_so_far = {start_info: 0}
        
        while frontier:
            current_priority, current_info = heapq.heappop(frontier)
            current_coord, current_dir, current_num_straight = current_info
            
            if current_coord == goal:
                return cost_so_far[current_info]
            
            for neighbor in self.neighbors4(current_coord):
                new_dir = neighbor.subtract(current_coord)
                new_num_straight = 1 if new_dir != current_dir else current_num_straight + 1
                neighbor_info = (neighbor, new_dir, new_num_straight)
                
                new_cost = cost_so_far[current_info] + self.data[neighbor]
                if neighbor_info not in cost_so_far or new_cost < cost_so_far[neighbor_info]:
                    if (current_num_straight >= min_straight or new_dir == current_dir or current_coord == start) and new_num_straight <= max_straight and new_dir != current_dir.opposite():
                        cost_so_far[neighbor_info] = new_cost
                        priority = new_cost + abs(neighbor.x - goal.x) + abs(neighbor.y - goal.y)
                        heapq.heappush(frontier, (priority, neighbor_info))
        
        return -1

def solve(input):
    grid = Grid(input)
    start = Coord(0, 0)
    goal = Coord(grid.width - 1, grid.height - 1)
    return grid.a_star_constrained(start, goal, 0, 3)

def read_file(file_name):
    with open(file_name, 'r') as file:
        return [line.strip() for line in file.readlines()]

input = read_file("input.txt")
print(solve(input))