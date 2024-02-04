file = open("input.txt", "r")
changes = file.readlines()
file.close()

frequency = 0
for change in changes:
    frequency += int(change)

print(frequency)