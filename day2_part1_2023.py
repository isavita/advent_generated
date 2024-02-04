
import re

# Read input from file
with open('input.txt', 'r') as file:
    data = file.readlines()

# Define target cube counts
target_cubes = {'red': 12, 'green': 13, 'blue': 14}

# Initialize sum of IDs of possible games
sum_of_ids = 0

# Iterate over each game
for game in data:
    game_id = int(re.search(r'\d+', game).group())
    cubes = re.findall(r'\d+ [a-z]+', game)
    
    game_possible = True
    for cube in cubes:
        count, color = cube.split()
        if target_cubes[color] < int(count):
            game_possible = False
            break
    
    if game_possible:
        sum_of_ids += game_id

print(sum_of_ids)
