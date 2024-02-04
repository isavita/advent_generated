
with open('input.txt', 'r') as file:
    rows = [[int(num) for num in line.split()] for line in file.readlines()]

checksum = sum([max(row) - min(row) for row in rows])

print(checksum)
