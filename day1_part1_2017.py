
#!/usr/bin/env python3
with open('input.txt', 'r') as f:
    data = f.read().strip()

sum = 0
for i in range(len(data)):
    if data[i] == data[(i + 1) % len(data)]:
        sum += int(data[i])

print(sum)
