with open("input.txt", "r") as file:
    strings = file.read().splitlines()

nice_count = 0

for string in strings:
    if any([string.count(char_pair) >= 2 for char_pair in [string[i:i+2] for i in range(len(string)-1)]]):
        if any([string[i] == string[i+2] for i in range(len(string)-2)]):
            nice_count += 1

print(nice_count)