import sys

def read_input(filename):
    """
    Reads the jet pattern from the input file.
    """
    with open(filename, 'r') as f:
        line = f.readline().strip()
    return line

def get_rock_shapes():
    """
    Defines the five rock shapes with their relative (x, y) coordinates.
    """
    rock_shapes = [
        # Horizontal Line (-)
        [(0,0), (1,0), (2,0), (3,0)],
        # Plus (+)
        [(1,0), (0,1), (1,1), (2,1), (1,2)],
        # Inverted L (⌝)
        [(0,0), (1,0), (2,0), (2,1), (2,2)],
        # Vertical Line (|)
        [(0,0), (0,1), (0,2), (0,3)],
        # Square (■)
        [(0,0), (1,0), (0,1), (1,1)]
    ]
    return rock_shapes

def can_move(rock, direction, chamber):
    """
    Checks if the rock can move in the specified direction without collision.
    Returns the new rock positions if movement is possible, else returns None.
    """
    moved_rock = []
    for (x, y) in rock:
        if direction == 'left':
            new_x, new_y = x - 1, y
        elif direction == 'right':
            new_x, new_y = x + 1, y
        elif direction == 'down':
            new_x, new_y = x, y - 1
        else:
            raise ValueError("Invalid direction")
        
        # Check chamber boundaries
        if new_x < 0 or new_x > 6 or new_y < 1:
            return None
        
        # Check for collision with settled rocks
        if (new_x, new_y) in chamber:
            return None
        
        moved_rock.append((new_x, new_y))
    
    return moved_rock

def get_chamber_profile(chamber, highest_y, depth=30):
    """
    Returns a tuple representing the profile of the chamber's top 'depth' rows.
    This helps in detecting cycles by capturing the shape of the chamber's surface.
    """
    profile = []
    for x in range(7):
        # Find the highest rock in this column
        for y in range(highest_y, highest_y - depth, -1):
            if (x, y) in chamber:
                profile.append(highest_y - y)
                break
        else:
            # No rock found in this column within the depth
            profile.append(highest_y + 1)
    return tuple(profile)

def simulate(jet_pattern, total_rocks):
    """
    Simulates the falling rocks influenced by the jet pattern.
    Returns the height of the tower after all rocks have settled.
    Utilizes cycle detection for efficiency.
    """
    rock_shapes = get_rock_shapes()
    chamber = set()
    
    # Initialize the chamber with the floor at y = 0
    for x in range(7):
        chamber.add((x, 0))
    
    highest_y = 0  # Tracks the highest y-coordinate in the chamber
    jet_len = len(jet_pattern)
    jet_index = 0  # Tracks the current position in the jet pattern
    rock_index = 0  # Tracks the current rock shape index
    
    # For cycle detection
    seen_states = {}
    additional_height = 0
    rock_number = 0
    
    while rock_number < total_rocks:
        # Determine the current rock shape
        shape = rock_shapes[rock_index % len(rock_shapes)]
        
        # Starting position: left edge 2 units from the left wall, bottom edge 3 units above the highest rock
        rock_x = 2
        rock_y = highest_y + 4
        rock = [(rock_x + dx, rock_y + dy) for (dx, dy) in shape]
        
        while True:
            # Apply jet push
            jet_dir = jet_pattern[jet_index % jet_len]
            jet_index += 1  # Move to the next jet
            
            if jet_dir == '>':
                moved_rock = can_move(rock, 'right', chamber)
                if moved_rock:
                    rock = moved_rock
            elif jet_dir == '<':
                moved_rock = can_move(rock, 'left', chamber)
                if moved_rock:
                    rock = moved_rock
            else:
                raise ValueError("Invalid jet direction in pattern.")
            
            # Attempt to move down
            moved_down = can_move(rock, 'down', chamber)
            if moved_down:
                rock = moved_down
            else:
                # Rock comes to rest
                for pos in rock:
                    chamber.add(pos)
                    if pos[1] > highest_y:
                        highest_y = pos[1]
                break  # Move to the next rock
        
        # Cycle detection
        profile = get_chamber_profile(chamber, highest_y)
        state = (rock_index % len(rock_shapes), jet_index % jet_len, profile)
        
        if state in seen_states and rock_number >= 2022:  # Ensure Part One is already covered if needed
            prev_rock, prev_height = seen_states[state]
            cycle_length = rock_number - prev_rock
            cycle_height = highest_y - prev_height
            
            remaining_rocks = total_rocks - rock_number
            num_cycles = remaining_rocks // cycle_length
            
            additional_height += num_cycles * cycle_height
            rock_number += num_cycles * cycle_length
        else:
            seen_states[state] = (rock_number, highest_y)
        
        rock_number += 1
        rock_index += 1
    
    return highest_y + additional_height

def main():
    jet_pattern = read_input('input.txt')
    total_rocks = 1000000000000  # 1,000,000,000,000 rocks
    final_height = simulate(jet_pattern, total_rocks)
    print(final_height)

if __name__ == "__main__":
    main()

