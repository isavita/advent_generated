
numbers = [int(line.strip()) for line in open("input.txt")]

invalid_num = 14360655

for i in range(len(numbers)):
    total = numbers[i]
    j = i + 1
    while total < invalid_num:
        total += numbers[j]
        j += 1
    if total == invalid_num:
        contiguous_set = numbers[i:j]
        weakness = min(contiguous_set) + max(contiguous_set)
        print(weakness)
        break
