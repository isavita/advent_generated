with open('input.txt', 'r') as file:
    data = file.read().splitlines()

count = 0
for i in range(1, len(data)):
    if int(data[i]) > int(data[i-1]):
        count += 1

print(count)