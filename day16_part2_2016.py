with open("input.txt", "r") as file:
    initial_state = file.read().strip()

def dragon_curve(a):
    b = ''.join('0' if c == '1' else '1' for c in reversed(a))
    return a + '0' + b

def generate_data(initial_state, disk_length):
    data = initial_state
    while len(data) < disk_length:
        data = dragon_curve(data)
    return data[:disk_length]

def calculate_checksum(data):
    checksum = ''
    for i in range(0, len(data), 2):
        if data[i] == data[i + 1]:
            checksum += '1'
        else:
            checksum += '0'
    if len(checksum) % 2 == 0:
        return calculate_checksum(checksum)
    else:
        return checksum

disk_length_1 = 272
disk_data_1 = generate_data(initial_state, disk_length_1)
checksum_1 = calculate_checksum(disk_data_1)
print(checksum_1)

disk_length_2 = 35651584
disk_data_2 = generate_data(initial_state, disk_length_2)
checksum_2 = calculate_checksum(disk_data_2)
print(checksum_2)