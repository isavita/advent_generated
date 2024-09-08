def dragon_curve(data, length):
    while len(data) < length:
        a = data
        b = ''.join('1' if c == '0' else '0' for c in reversed(a))
        data = a + '0' + b
    return data[:length]

def calculate_checksum(data):
    checksum = data
    while len(checksum) % 2 == 0:
        new_checksum = ''
        for i in range(0, len(checksum), 2):
            new_checksum += '1' if checksum[i] == checksum[i+1] else '0'
        checksum = new_checksum
    return checksum

def solve(initial_state, disk_length):
    data = dragon_curve(initial_state, disk_length)
    return calculate_checksum(data)

# Your puzzle input
initial_state = "10001001100010110"

# Part 1
print("Part 1:", solve(initial_state, 272))

# Part 2
print("Part 2:", solve(initial_state, 35651584))
