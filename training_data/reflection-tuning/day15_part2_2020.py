def play_memory_game(starting_numbers, target_turn):
    last_spoken = {num: i + 1 for i, num in enumerate(starting_numbers[:-1])}
    last_number = starting_numbers[-1]

    for turn in range(len(starting_numbers), target_turn):
        if last_number in last_spoken:
            next_number = turn - last_spoken[last_number]
        else:
            next_number = 0
        
        last_spoken[last_number] = turn
        last_number = next_number

    return last_number

# Read input from file
with open('input.txt', 'r') as file:
    starting_numbers = [int(x) for x in file.read().strip().split(',')]

# Part 1
print(play_memory_game(starting_numbers, 2020))

# Part 2
print(play_memory_game(starting_numbers, 30000000))
