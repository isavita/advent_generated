def simulate_rocks(jet_pattern, num_rocks):
    chamber = set()
    height = 0
    jet_index = 0
    
    rocks = [
        [(0,0), (1,0), (2,0), (3,0)],  # horizontal line
        [(1,0), (0,1), (1,1), (2,1), (1,2)],  # plus
        [(0,0), (1,0), (2,0), (2,1), (2,2)],  # L-shape
        [(0,0), (0,1), (0,2), (0,3)],  # vertical line
        [(0,0), (1,0), (0,1), (1,1)]  # square
    ]
    
    def is_valid_position(rock, pos):
        for dx, dy in rock:
            x, y = pos[0] + dx, pos[1] + dy
            if x < 0 or x >= 7 or y <= 0 or (x, y) in chamber:
                return False
        return True
    
    for i in range(num_rocks):
        rock = rocks[i % 5]
        pos = [2, height + 4]
        
        while True:
            # Jet push
            direction = 1 if jet_pattern[jet_index] == '>' else -1
            jet_index = (jet_index + 1) % len(jet_pattern)
            
            if is_valid_position(rock, (pos[0] + direction, pos[1])):
                pos[0] += direction
            
            # Fall down
            if is_valid_position(rock, (pos[0], pos[1] - 1)):
                pos[1] -= 1
            else:
                for dx, dy in rock:
                    chamber.add((pos[0] + dx, pos[1] + dy))
                    height = max(height, pos[1] + dy)
                break
    
    return height

# Read input
with open('input.txt', 'r') as file:
    jet_pattern = file.read().strip()

# Simulate 2022 rocks
result = simulate_rocks(jet_pattern, 2022)
print(result)
