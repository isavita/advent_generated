from collections import deque
import copy

def intcode_computer(program, input_value):
    # ... (implementation of intcode_computer remains the same)
    # Ensure this function always returns a valid status code (0, 1, or 2)

def explore_map(program):
    map_data = {(0, 0): '.'}
    oxygen_pos = None
    queue = deque([(0, 0, copy.deepcopy(program), 0)])
    directions = [(0, 1), (0, -1), (-1, 0), (1, 0)]
    steps_to_oxygen = 0

    while queue:
        x, y, prog, steps = queue.popleft()

        for i, (dx, dy) in enumerate(directions):
            nx, ny = x + dx, y + dy
            if (nx, ny) not in map_data:
                new_prog = copy.deepcopy(prog)
                status = intcode_computer(new_prog, i + 1)
                
                # Add a check for None status
                if status is None:
                    status = 0  # Treat None as if it hit a wall

                if status > 0:
                    map_data[(nx, ny)] = '.'
                    queue.append((nx, ny, new_prog, steps + 1))
                    if status == 2:
                        oxygen_pos = (nx, ny)
                        steps_to_oxygen = steps + 1
                elif status == 0:
                    map_data[(nx, ny)] = '#'

    return map_data, oxygen_pos, steps_to_oxygen

def fill_with_oxygen(map_data, start_pos):
    queue = deque([(start_pos, 0)])
    visited = set([start_pos])
    max_time = 0

    while queue:
        pos, time = queue.popleft()
        max_time = max(max_time, time)

        for dx, dy in [(0, 1), (0, -1), (-1, 0), (1, 0)]:
            nx, ny = pos[0] + dx, pos[1] + dy
            if (nx, ny) not in visited and map_data.get((nx, ny)) == '.':
                visited.add((nx, ny))
                queue.append(((nx, ny), time + 1))

    return max_time

def main():
    with open('input.txt', 'r') as file:
        program = list(map(int, file.read().strip().split(',')))

    map_data, oxygen_pos, steps_to_oxygen = explore_map(program)
    print(f"Part 1: {steps_to_oxygen}")

    time_to_fill = fill_with_oxygen(map_data, oxygen_pos)
    print(f"Part 2: {time_to_fill}")

if __name__ == "__main__":
    main()
