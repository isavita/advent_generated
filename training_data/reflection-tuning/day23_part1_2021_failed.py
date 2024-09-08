from collections import deque
import heapq

# Constants for clarity and to avoid magic numbers
HALLWAY_LENGTH = 11
ROOM_DEPTH = 2
ROOM_POSITIONS = [2, 4, 6, 8]
ENERGY = {'A': 1, 'B': 10, 'C': 100, 'D': 1000}
TARGET_ROOMS = {'A': 0, 'B': 1, 'C': 2, 'D': 3}

def is_path_clear(state, start, end):
    step = 1 if start < end else -1
    return all(state[i] is None for i in range(start + step, end + step, step))

def room_available(state, amphipod, room_index):
    start = HALLWAY_LENGTH + room_index * ROOM_DEPTH
    return all(state[i] in (None, amphipod) for i in range(start, start + ROOM_DEPTH))

def generate_moves(state):
    # Generate moves from hallway to room
    for i, amphipod in enumerate(state[:HALLWAY_LENGTH]):
        if amphipod:
            target_room = TARGET_ROOMS[amphipod]
            room_pos = ROOM_POSITIONS[target_room]
            if is_path_clear(state, i, room_pos) and room_available(state, amphipod, target_room):
                room_index = HALLWAY_LENGTH + target_room * ROOM_DEPTH
                for depth in range(ROOM_DEPTH - 1, -1, -1):
                    if state[room_index + depth] is None:
                        yield (i, room_index + depth)
                        break

    # Generate moves from room to hallway
    for room in range(4):
        room_index = HALLWAY_LENGTH + room * ROOM_DEPTH
        for depth in range(ROOM_DEPTH):
            pos = room_index + depth
            if state[pos]:
                if depth == 0 or any(state[room_index + j] is not None for j in range(depth)):
                    amphipod = state[pos]
                    if not room_available(state, amphipod, room):
                        for hallway_pos in [0, 1, 3, 5, 7, 9, 10]:
                            if is_path_clear(state, ROOM_POSITIONS[room], hallway_pos):
                                yield (pos, hallway_pos)
                break

def move_cost(state, start, end):
    amphipod = state[start]
    return ENERGY[amphipod] * (abs(start % HALLWAY_LENGTH - end % HALLWAY_LENGTH) + 
                               (start // HALLWAY_LENGTH) + (end // HALLWAY_LENGTH))

def is_organized(state):
    return all(state[HALLWAY_LENGTH + i * ROOM_DEPTH + j] == 'ABCD'[i] 
               for i in range(4) for j in range(ROOM_DEPTH))

def solve(initial_state):
    queue = [(0, initial_state)]
    seen = set()
    
    while queue:
        energy, state = heapq.heappop(queue)
        
        if is_organized(state):
            return energy
        
        if state in seen:
            continue
        seen.add(state)
        
        for start, end in generate_moves(state):
            new_state = list(state)
            new_state[start], new_state[end] = new_state[end], new_state[start]
            new_state = tuple(new_state)
            new_energy = energy + move_cost(state, start, end)
            heapq.heappush(queue, (new_energy, new_state))
    
    return float('inf')

# Example usage
initial_state = (None,) * HALLWAY_LENGTH + ('B', 'A', 'C', 'D', 'B', 'C', 'D', 'A')
print(solve(initial_state))
