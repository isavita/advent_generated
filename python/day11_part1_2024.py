
def even_digits(s):
    return len(s) % 2 == 0

def trim_leading_zeros(s):
    s = s.lstrip('0')
    return s or "0"

with open("input.txt", "r") as f:
    line = f.readline().strip()
    stones = line.split()

for _ in range(25):
    next_stones = []
    for s in stones:
        if s == "0":
            next_stones.append("1")
        elif even_digits(s):
            mid = len(s) // 2
            left = trim_leading_zeros(s[:mid])
            right = trim_leading_zeros(s[mid:])
            next_stones.extend([left, right])
        else:
            next_stones.append(str(int(s) * 2024))
    stones = next_stones

print(len(stones))

