import sys

def read_input(file_path):
    elves = set()
    with open(file_path, 'r') as file:
        for y, line in enumerate(file):
            line = line.strip()
            for x, char in enumerate(line):
                if char == '#':
                    elves.add((x, y))
    return elves

def get_adjacent_positions(x, y):
    return [
        (x-1, y-1), (x, y-1), (x+1, y-1),
        (x-1, y),           (x+1, y),
        (x-1, y+1), (x, y+1), (x+1, y+1)
    ]

def simulate_rounds(elves, rounds):
    # Define directions with their movement vectors and the positions they check
    directions = {
        'N': {'move': (0, -1), 'checks': [ (0, -1), (1, -1), (-1, -1) ]},
        'S': {'move': (0, 1), 'checks': [ (0, 1), (1, 1), (-1, 1) ]},
        'W': {'move': (-1, 0), 'checks': [ (-1, 0), (-1, -1), (-1, 1) ]},
        'E': {'move': (1, 0), 'checks': [ (1, 0), (1, -1), (1, 1) ]}
    }

    # Initial order of directions
    directions_order = ['N', 'S', 'W', 'E']

    for round_num in range(1, rounds + 1):
        proposals = {}
        proposed_moves = {}

        # First half: each Elf proposes a move
        for elf in elves:
            x, y = elf
            adjacent = get_adjacent_positions(x, y)
            has_neighbor = any(pos in elves for pos in adjacent)
            if not has_neighbor:
                continue  # No move proposed

            # Iterate over directions in current order
            for direction in directions_order:
                can_move = True
                for dx, dy in directions[direction]['checks']:
                    check_pos = (x + dx, y + dy)
                    if check_pos in elves:
                        can_move = False
                        break
                if can_move:
                    move_dx, move_dy = directions[direction]['move']
                    dest = (x + move_dx, y + move_dy)
                    proposals.setdefault(dest, []).append(elf)
                    proposed_moves[elf] = dest
                    break  # Stop after proposing a move

        # Second half: execute the moves
        new_elves = set(elves)
        for dest, proposers in proposals.items():
            if len(proposers) == 1:
                proposer = proposers[0]
                new_elves.remove(proposer)
                new_elves.add(dest)
        elves = new_elves

        # Rotate the directions order
        directions_order = directions_order[1:] + directions_order[:1]

    return elves

def calculate_empty_ground(elves):
    min_x = min(x for x, y in elves)
    max_x = max(x for x, y in elves)
    min_y = min(y for x, y in elves)
    max_y = max(y for x, y in elves)

    width = max_x - min_x + 1
    height = max_y - min_y + 1
    total_tiles = width * height
    empty_tiles = total_tiles - len(elves)
    return empty_tiles

def main():
    input_file = 'input.txt'
    elves = read_input(input_file)
    elves = simulate_rounds(elves, 10)
    empty_ground = calculate_empty_ground(elves)
    print(empty_ground)

if __name__ == "__main__":
    main()
