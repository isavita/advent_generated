import heapq

def read_input(file_path):
    """
    Reads the input file and parses the grid.

    Args:
        file_path (str): Path to the input file.

    Returns:
        grid (list of list of int): 2D grid representing the heat loss values.
    """
    grid = []
    with open(file_path, 'r') as file:
        for line in file:
            grid.append([int(char) for char in line.strip()])
    return grid

def get_direction_changes():
    """
    Defines the possible direction changes: left and right turns.

    Returns:
        direction_turns (dict): Mapping of each direction to its left and right turns.
    """
    direction_turns = {
        'N': {'L': 'W', 'R': 'E'},
        'S': {'L': 'E', 'R': 'W'},
        'E': {'L': 'N', 'R': 'S'},
        'W': {'L': 'S', 'R': 'N'}
    }
    return direction_turns

def get_directions():
    """
    Defines the movement vectors for each direction.

    Returns:
        directions (dict): Mapping of direction names to their (dx, dy) vectors.
    """
    directions = {
        'N': (0, -1),
        'S': (0, 1),
        'E': (1, 0),
        'W': (-1, 0)
    }
    return directions

def dijkstra(grid, part):
    """
    Implements Dijkstra's algorithm to find the least heat loss path.

    Args:
        grid (list of list of int): 2D grid representing the heat loss values.
        part (int): Part of the challenge to solve (1 or 2).

    Returns:
        least_heat_loss (int): The minimum total heat loss to reach the destination.
    """
    height = len(grid)
    width = len(grid[0])
    start = (0, 0)
    end = (width - 1, height - 1)

    directions = get_directions()
    direction_turns = get_direction_changes()

    # Initialize priority queue
    # Each element is a tuple: (total_heat_loss, x, y, direction, steps_in_current_direction)
    heap = []
    initial_direction = 'E'  # Starting direction: Right
    initial_state = (0, start[0], start[1], initial_direction, 0)
    heapq.heappush(heap, initial_state)

    # Visited dictionary to keep track of the minimum heat loss for each state
    # Key: (x, y, direction, steps_in_current_direction)
    # Value: total_heat_loss
    visited = {}

    while heap:
        total_heat_loss, x, y, direction, steps = heapq.heappop(heap)

        # If reached the destination, return the total_heat_loss
        if (x, y) == end:
            return total_heat_loss

        # Define the state key
        state_key = (x, y, direction, steps)

        # If this state has been visited with a lower or equal heat loss, skip
        if state_key in visited and visited[state_key] <= total_heat_loss:
            continue

        # Mark this state as visited
        visited[state_key] = total_heat_loss

        # Determine possible next directions based on the current part's constraints
        possible_moves = []

        if part == 1:
            max_steps = 3
            min_steps_before_turn = 1  # Can turn any time after at least 1 step
        elif part == 2:
            max_steps = 10
            min_steps_before_turn = 4  # Must move at least 4 steps before turning
        else:
            raise ValueError("Invalid part specified. Choose 1 or 2.")

        # Check if we can continue moving in the same direction
        if steps < max_steps:
            possible_moves.append(('C', direction))  # Continue straight

        # Check if we can turn left or right
        if steps >= min_steps_before_turn:
            for turn in ['L', 'R']:
                new_direction = direction_turns[direction][turn]
                possible_moves.append((turn, new_direction))

        # Iterate over possible moves
        for move, new_direction in possible_moves:
            dx, dy = directions[new_direction]
            nx, ny = x + dx, y + dy

            # Check boundaries
            if not (0 <= nx < width and 0 <= ny < height):
                continue  # Out of bounds

            # Prevent reversing direction
            reverse_directions = {
                'N': 'S',
                'S': 'N',
                'E': 'W',
                'W': 'E'
            }
            if new_direction == reverse_directions.get(direction, ''):
                continue  # Cannot reverse direction

            # Calculate new steps_in_current_direction
            if move == 'C':
                new_steps = steps + 1
            else:
                new_steps = 1  # Reset steps after a turn

            # Calculate new total_heat_loss
            new_heat_loss = total_heat_loss + grid[ny][nx]

            # Push the new state into the heap
            new_state = (new_heat_loss, nx, ny, new_direction, new_steps)
            heapq.heappush(heap, new_state)

    # If destination is not reachable
    return -1

def main():
    input_file = 'input.txt'
    grid = read_input(input_file)

    # Part One
    least_heat_loss_part_one = dijkstra(grid, part=1)
    print("Part One Answer:", least_heat_loss_part_one)

    # Part Two
    least_heat_loss_part_two = dijkstra(grid, part=2)
    print("Part Two Answer:", least_heat_loss_part_two)

if __name__ == "__main__":
    main()
