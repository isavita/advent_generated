
with open('input.txt') as f:
    data = [list(map(int, line.strip().split('-'))) for line in f.readlines()]

data.sort()

current_max = 0
allowed_ips = 0

for start, end in data:
    if start > current_max:
        allowed_ips += start - current_max - 1
    current_max = max(current_max, end)

allowed_ips += 4294967295 - current_max

print(allowed_ips)
