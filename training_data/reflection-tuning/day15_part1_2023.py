def hash_algorithm(s):
    current_value = 0
    for char in s:
        current_value += ord(char)
        current_value *= 17
        current_value %= 256
    return current_value

# Read input from file
with open('input.txt', 'r') as file:
    initialization_sequence = file.read().strip()

# Split the sequence and apply HASH algorithm to each step
steps = initialization_sequence.split(',')
result = sum(hash_algorithm(step) for step in steps)

print(result)
