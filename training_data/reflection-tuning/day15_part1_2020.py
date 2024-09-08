def find_2020th_number(starting_numbers):
    # Initialize the dictionary with starting numbers
    last_spoken = {num: i+1 for i, num in enumerate(starting_numbers)}
    last_number = starting_numbers[-1]
    
    # Iterate from len(starting_numbers) + 1 to 2020
    for turn in range(len(starting_numbers) + 1, 2021):
        # Check if the last number has been spoken before
        if last_number in last_spoken and last_spoken[last_number] < turn - 1:
            next_number = turn - 1 - last_spoken[last_number]
        else:
            next_number = 0
        
        # Update the dictionary
        last_spoken[last_number] = turn - 1
        last_number = next_number
    
    return last_number

# Read input from file
with open('input.txt', 'r') as file:
    starting_numbers = [int(x) for x in file.read().strip().split(',')]

# Calculate and print the result
result = find_2020th_number(starting_numbers)
print(result)
