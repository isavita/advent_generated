with open('input.txt', 'r') as file:
    data = list(map(int, file.read().split(',')))

data[1] = 12
data[2] = 2

pos = 0
while data[pos] != 99:
    if data[pos] == 1:
        data[data[pos + 3]] = data[data[pos + 1]] + data[data[pos + 2]]
    elif data[pos] == 2:
        data[data[pos + 3]] = data[data[pos + 1]] * data[data[pos + 2]]
    pos += 4

print(data[0])