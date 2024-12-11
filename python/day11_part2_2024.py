
import sys

def trim_leading_zeros(s):
    i = 0
    while i < len(s) - 1 and s[i] == '0':
        i += 1
    return s[i:]

def split_stone(s):
    mid = len(s) // 2
    left = trim_leading_zeros(s[:mid])
    right = trim_leading_zeros(s[mid:])
    return left or "0", right or "0"

def multiply_by_2024(s):
    num = [int(c) for c in s]
    multiplier = [2, 0, 2, 4]
    result = [0] * (len(num) + len(multiplier))
    for i in range(len(num) - 1, -1, -1):
        carry = 0
        for j in range(len(multiplier) - 1, -1, -1):
            product = num[i] * multiplier[j] + result[i + j + 1] + carry
            result[i + j + 1] = product % 10
            carry = product // 10
        result[i] += carry
    start = 0
    while start < len(result) - 1 and result[start] == 0:
        start += 1
    return "".join(map(str, result[start:]))

def solve():
    try:
        with open("input.txt", "r") as f:
            line = f.readline().strip()
    except FileNotFoundError:
        print("Error: input.txt not found.")
        sys.exit(1)

    stones = line.split()
    stones_map = {}
    for stone in stones:
        stones_map[stone] = stones_map.get(stone, 0) + 1

    steps = 75
    for _ in range(steps):
        new_stones_map = {}
        for stone, count in stones_map.items():
            if stone == "0":
                new_stones_map["1"] = new_stones_map.get("1", 0) + count
            elif len(stone) % 2 == 0:
                left, right = split_stone(stone)
                new_stones_map[left] = new_stones_map.get(left, 0) + count
                new_stones_map[right] = new_stones_map.get(right, 0) + count
            else:
                new_stone = multiply_by_2024(stone)
                new_stones_map[new_stone] = new_stones_map.get(new_stone, 0) + count
        stones_map = new_stones_map

    total_stones = sum(stones_map.values())
    print(total_stones)

if __name__ == "__main__":
    solve()

