
with open('input.txt', 'r') as file:
    data = file.read().strip()

def find_start_of_packet(data):
    for i in range(3, len(data)):
        if len(set(data[i-3:i+1])) == 4:
            return i + 1

result = find_start_of_packet(data)
print(result)
