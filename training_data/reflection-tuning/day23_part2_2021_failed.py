from collections import defaultdict
import heapq

# Constants
ROOM_DEPTH = 2  # Change this to 4 for Part 2
ROOM_POSITIONS = [2, 4, 6, 8]
HALLWAY_POSITIONS = [0, 1, 3, 5, 7, 9, 10]
ENERGY = {'A': 1, 'B': 10, 'C': 100, 'D': 1000}
ROOM_INDEX = {'A': 0, 'B': 1, 'C': 2, 'D': 3}

def parse_input(input_str):
    lines = input_str.strip().split('\n')
    state = ['.'] * 11  # Hallway
    for i in range(ROOM_DEPTH):
        for j, pos in enumerate(ROOM_POSITIONS):
            state.append(lines[i+2][pos+1])
    return tuple(state)

def is_organized(state):
    for i, pos in enumerate(ROOM_POSITIONS):
        for depth in range(ROOM_DEPTH):
            if state[11 + i + depth * 4] != chr(ord('A') + i):
                return False
    return True

def get_valid_moves(state):
    moves = []

    # Move from hallway to room
    for i, amphipod in enumerate(state[:11]):
        if amphipod in 'ABCD':
            target_room = ROOM_INDEX[amphipod]
            target_pos = ROOM_POSITIONS[target_room]
            
            # Check if path to room is clear
            path = range(min(i, target_pos), max(i, target_pos) + 1)
            if all(state[j] == '.' for j in path if j != i):
                # Check if room is available
                room_start = 11 + target_room
                if all(state[room_start + j] in ('.', amphipod) for j in range(ROOM_DEPTH)):
                    # Find the deepest available spot in the room
                    for depth in range(ROOM_DEPTH - 1, -1, -1):
                        if state[room_start + depth] == '.':
                            new_state = list(state)
                            new_state[i] = '.'
                            new_state[room_start + depth] = amphipod
                            cost = (abs(i - target_pos) + depth + 1) * ENERGY[amphipod]
                            moves.append((tuple(new_state), cost))
                            break

    # Move from room to hallway
    for i, pos in enumerate(ROOM_POSITIONS):
        room_start = 11 + i
        for depth in range(ROOM_DEPTH):
            amphipod = state[room_start + depth]
            if amphipod in 'ABCD':
                if ROOM_INDEX[amphipod] != i or any(state[room_start + j] not in ('.', amphipod) for j in range(depth + 1, ROOM_DEPTH)):
                    for hall_pos in HALLWAY_POSITIONS:
                        path = range(min(pos, hall_pos), max(pos, hall_pos) + 1)
                        if all(state[j] == '.' for j in path):
                            new_state = list(state)
                            new_state[room_start + depth] = '.'
                            new_state[hall_pos] = amphipod
                            cost = (abs(pos - hall_pos) + depth + 1) * ENERGY[amphipod]
                            moves.append((tuple(new_state), cost))
                break  # Only consider topmost amphipod in each room

    return moves

def solve(initial_state):
    queue = [(0, initial_state)]
    seen = set()
    
    while queue:
        cost, state = heapq.heappop(queue)
        
        if is_organized(state):
            return cost
        
        if state in seen:
            continue
        seen.add(state)
        
        for next_state, move_cost in get_valid_moves(state):
            heapq.heappush(queue, (cost + move_cost, next_state))
    
    return float('inf')

# Example usage
initial_state = parse_input("""
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
""")

print(f"Part 1: {solve(initial_state)}")

# For Part 2, modify the ROOM_DEPTH constant and update the parse_input function
# to handle the additional two lines of amphipods
