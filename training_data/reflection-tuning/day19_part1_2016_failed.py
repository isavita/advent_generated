from collections import deque

def find_winner(num_elves):
    elves = deque(range(1, num_elves + 1))
    
    while len(elves) > 1:
        elves.rotate(-1)  # Move the current elf to the end
        elves.popleft()   # Remove the elf who lost their present
    
    return elves[0]

# Puzzle input
num_elves = 3014387

winner = find_winner(num_elves)
print(winner)
