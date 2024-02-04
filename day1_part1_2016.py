
dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]
current_dir = 0
x, y = 0, 0

with open('input.txt', 'r') as f:
    data = f.read().split(', ')
    
for d in data:
    if d[0] == 'R':
        current_dir = (current_dir + 1) % 4
    else:
        current_dir = (current_dir - 1) % 4
        
    dist = int(d[1:])
    x += dirs[current_dir][0] * dist
    y += dirs[current_dir][1] * dist
    
print(abs(x) + abs(y))
