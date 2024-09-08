def roll_die(current_roll):
    return (current_roll - 1) % 100 + 1

def play_game(p1_start, p2_start):
    positions = [p1_start, p2_start]
    scores = [0, 0]
    current_roll = 1
    total_rolls = 0
    current_player = 0

    while max(scores) < 1000:
        move = sum(roll_die(current_roll + i) for i in range(3))
        current_roll = (current_roll + 2) % 100 + 1
        total_rolls += 3

        positions[current_player] = (positions[current_player] + move - 1) % 10 + 1
        scores[current_player] += positions[current_player]
        
        if scores[current_player] >= 1000:
            break
        
        current_player = 1 - current_player

    losing_score = min(scores)
    return losing_score * total_rolls

# Read input from file
with open('input.txt', 'r') as f:
    p1_start = int(f.readline().split()[-1])
    p2_start = int(f.readline().split()[-1])

# Play the game and print the result
result = play_game(p1_start, p2_start)
print(result)
