import hashlib

def get_hash(input_string):
    return hashlib.md5(input_string.encode()).hexdigest()

def get_open_doors(hash_str):
    doors = []
    for char in hash_str[:4]:
        if char in 'bcdef':
            doors.append(True)
        else:
            doors.append(False)
    return doors

def is_valid_move(x, y, direction):
    if direction == 'U':
        return y > 0
    elif direction == 'D':
        return y < 3
    elif direction == 'L':
        return x > 0
    elif direction == 'R':
        return x < 3

def find_shortest_path(passcode):
    directions = ['U', 'D', 'L', 'R']
    queue = [(0, 0, '')]  # (x, y, path)
    
    while queue:
        x, y, path = queue.pop(0)
        
        if x == 3 and y == 3:
            return path
        
        hash_input = passcode + path
        hash_str = get_hash(hash_input)
        open_doors = get_open_doors(hash_str)
        
        for i in range(4):
            if open_doors[i] and is_valid_move(x, y, directions[i]):
                new_x = x
                new_y = y
                if directions[i] == 'U':
                    new_y -= 1
                elif directions[i] == 'D':
                    new_y += 1
                elif directions[i] == 'L':
                    new_x -= 1
                elif directions[i] == 'R':
                    new_x += 1
                queue.append((new_x, new_y, path + directions[i]))

# Read input from file
with open('input.txt', 'r') as file:
    passcode = file.read().strip()

# Find the shortest path to reach the vault
shortest_path = find_shortest_path(passcode)
print(shortest_path)