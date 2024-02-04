with open('input.txt', 'r') as file:
    data = file.read().strip()

def find_marker(data, marker_length):
    for i in range(marker_length, len(data)):
        if len(set(data[i-marker_length:i])) == marker_length:
            return i

print(find_marker(data, 14))