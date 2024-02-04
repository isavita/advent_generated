
with open('input.txt', 'r') as file:
    start, end = map(int, file.readline().strip().split('-'))

count = 0
for num in range(start, end+1):
    digits = [int(d) for d in str(num)]
    if sorted(digits) == digits and any(digits.count(d) >= 2 for d in set(digits)):
        count += 1

print(count)
