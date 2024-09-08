from typing import List, Tuple

ROCKS = [
    [(0,0), (1,0), (2,0), (3,0)],
    [(1,0), (0,1), (1,1), (2,1), (1,2)],
    [(0,0), (1,0), (2,0), (2,1), (2,2)],
    [(0,0), (0,1), (0,2), (0,3)],
    [(0,0), (1,0), (0,1), (1,1)]
]

def simulate(jet_pattern: str, num_rocks: int) -> int:
    chamber = set()
    height = 0
    jet_index = 0
    
    for rock_index in range(num_rocks):
        rock = ROCKS[rock_index % len(ROCKS)]
        rock_pos = [2, height + 3]
        
        while True:
            # Jet push
            direction = 1 if jet_pattern[jet_index % len(jet_pattern)] == '>' else -1
            jet_index += 1
            
            if all(0 <= x + direction < 7 and (x + direction, y) not in chamber for x, y in rock):
                rock_pos[0] += direction
            
            # Fall down
            if all(y > 0 and (x, y - 1) not in chamber for x, y in [(x + rock_pos[0], y + rock_pos[1]) for x, y in rock]):
                rock_pos[1] -= 1
            else:
                break
        
        # Settle rock
        for x, y in rock:
            chamber.add((x + rock_pos[0], y + rock_pos[1]))
            height = max(height, y + rock_pos[1] + 1)
    
    return height

def find_cycle(jet_pattern: str) -> Tuple[int, int, int]:
    chamber = set()
    height = 0
    jet_index = 0
    states = {}
    
    for rock_index in range(1000000):
        rock = ROCKS[rock_index % len(ROCKS)]
        rock_pos = [2, height + 3]
        
        while True:
            direction = 1 if jet_pattern[jet_index % len(jet_pattern)] == '>' else -1
            jet_index += 1
            
            if all(0 <= x + direction < 7 and (x + direction, y) not in chamber for x, y in rock):
                rock_pos[0] += direction
            
            if all(y > 0 and (x, y - 1) not in chamber for x, y in [(x + rock_pos[0], y + rock_pos[1]) for x, y in rock]):
                rock_pos[1] -= 1
            else:
                break
        
        for x, y in rock:
            chamber.add((x + rock_pos[0], y + rock_pos[1]))
            height = max(height, y + rock_pos[1] + 1)
        
        state = (rock_index % len(ROCKS), jet_index % len(jet_pattern), frozenset((x, height - y) for x, y in chamber if height - y <= 30))
        if state in states:
            prev_rock_index, prev_height = states[state]
            return prev_rock_index, rock_index - prev_rock_index, height - prev_height
        
        states[state] = (rock_index, height)
    
    return 0, 0, 0

def solve_part1(jet_pattern: str) -> int:
    return simulate(jet_pattern, 2022)

def solve_part2(jet_pattern: str) -> int:
    start, cycle_length, cycle_height = find_cycle(jet_pattern)
    target = 1000000000000
    
    full_cycles = (target - start) // cycle_length
    remainder = (target - start) % cycle_length
    
    base_height = simulate(jet_pattern, start + remainder)
    return base_height + full_cycles * cycle_height

def main():
    with open('input.txt', 'r') as f:
        jet_pattern = f.read().strip()
    
    print(f"Part 1: {solve_part1(jet_pattern)}")
    print(f"Part 2: {solve_part2(jet_pattern)}")

if __name__ == "__main__":
    main()
