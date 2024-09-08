def generate_data(initial, length):
    data = initial
    while len(data) < length:
        b = ''.join('1' if c == '0' else '0' for c in data[::-1])
        data = data + '0' + b
    return data[:length]

def calculate_checksum(data):
    checksum = data
    while len(checksum) % 2 == 0:
        checksum = ''.join('1' if checksum[i] == checksum[i+1] else '0' for i in range(0, len(checksum), 2))
    return checksum

def solve(initial, length):
    data = generate_data(initial, length)
    return calculate_checksum(data)

# Read input from file
with open('input.txt', 'r') as file:
    initial_state = file.read().strip()

# Solve the problem
result = solve(initial_state, 272)
print(result)
