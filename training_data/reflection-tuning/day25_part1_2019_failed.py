from collections import deque
from intcode import IntcodeComputer  # Assuming we have an IntcodeComputer class

def explore_ship(program):
    computer = IntcodeComputer(program)
    visited = set()
    items = set()
    inventory = set()
    queue = deque([('', [])])  # (path, commands)

    while queue:
        path, commands = queue.popleft()
        
        # Execute commands
        for cmd in commands:
            computer.add_input(ord(c) for c in cmd + '\n')
        
        output = ''
        while True:
            result = computer.run()
            if result is None:
                break
            output += chr(result)
        
        if "Command?" in output:
            room = output.split('\n')[0]
            if room not in visited:
                visited.add(room)
                
                # Check for items
                for line in output.split('\n'):
                    if line.startswith('Items here:'):
                        item = line.split('-')[1].strip()
                        if item not in items and item != "infinite loop":
                            items.add(item)
                            queue.append((path, commands + [f'take {item}']))
                
                # Explore directions
                for direction in ['north', 'south', 'east', 'west']:
                    queue.append((path + direction, commands + [direction]))
            
            # Check if we've found the password
            if "You should be able to get in by typing" in output:
                return output.split("typing")[1].strip()

    return "Password not found"

# Main execution
with open('input.txt', 'r') as file:
    program = [int(x) for x in file.read().strip().split(',')]

password = explore_ship(program)
print(f"The password for the main airlock is: {password}")
