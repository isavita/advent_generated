with open('input.txt') as f:
    directions = f.read().strip()

def build_map(directions):
    stack = []
    rooms = {}
    current_room = (0, 0)
    doors_passed = {current_room: 0}
    
    for char in directions[1:-1]:
        if char == '(':
            stack.append((current_room, doors_passed.copy()))
        elif char == '|':
            current_room, doors_passed = stack[-1]
        elif char == ')':
            current_room, doors_passed = stack.pop()
        else:
            dx, dy = {
                'N': (0, -1),
                'E': (1, 0),
                'S': (0, 1),
                'W': (-1, 0)
            }[char]
            new_room = (current_room[0] + dx, current_room[1] + dy)
            doors_passed[new_room] = doors_passed[current_room] + 1
            rooms.setdefault(new_room, doors_passed[new_room])
            if doors_passed[new_room] < rooms[new_room]:
                rooms[new_room] = doors_passed[new_room]
            current_room = new_room
            
    return rooms

rooms = build_map(directions)
max_doors = max(rooms.values())
rooms_with_1000_doors = sum(1 for doors in rooms.values() if doors >= 1000)

print(max_doors)
print(rooms_with_1000_doors)