with open("input.txt") as f:
    favorite_number = int(f.read().strip())

def is_wall(x, y):
    num = x*x + 3*x + 2*x*y + y + y*y + favorite_number
    return bin(num).count('1') % 2 != 0

def bfs(start, target):
    queue = [(start, 0)]
    visited = set()
    
    while queue:
        (x, y), steps = queue.pop(0)
        if (x, y) == target:
            return steps
        
        if (x, y) in visited or is_wall(x, y):
            continue
        
        visited.add((x, y))
        
        for dx, dy in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            new_x, new_y = x + dx, y + dy
            if new_x >= 0 and new_y >= 0:
                queue.append(((new_x, new_y), steps + 1))

start = (1, 1)
target = (31, 39)
result = bfs(start, target)
print(result)