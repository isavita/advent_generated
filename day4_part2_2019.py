with open("input.txt", "r") as file:
    data = file.read().split("-")
    start = int(data[0])
    end = int(data[1])

def has_adjacent(num):
    num_str = str(num)
    for i in range(len(num_str)-1):
        if num_str[i] == num_str[i+1]:
            return True
    return False

def has_only_two_adjacent(num):
    num_str = str(num)
    count = 1
    for i in range(len(num_str)-1):
        if num_str[i] == num_str[i+1]:
            count += 1
        else:
            if count == 2:
                return True
            count = 1
    if count == 2:
        return True
    return False

def never_decreases(num):
    num_str = str(num)
    for i in range(len(num_str)-1):
        if int(num_str[i]) > int(num_str[i+1]):
            return False
    return True

count_part1 = 0
count_part2 = 0

for i in range(start, end+1):
    if has_adjacent(i) and never_decreases(i):
        count_part1 += 1
    if has_only_two_adjacent(i) and never_decreases(i):
        count_part2 += 1

print(count_part1)
print(count_part2)