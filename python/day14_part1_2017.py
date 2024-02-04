with open("input.txt", "r") as file:
    key = file.read().strip()

def knot_hash(input):
    lengths = [ord(char) for char in input] + [17, 31, 73, 47, 23]
    nums = list(range(256))
    pos = skip = 0

    for _ in range(64):
        for length in lengths:
            for i in range(length // 2):
                a = (pos + i) % 256
                b = (pos + length - 1 - i) % 256
                nums[a], nums[b] = nums[b], nums[a]
            pos += length + skip
            skip += 1

    dense_hash = []
    for i in range(16):
        xored = nums[i * 16]
        for j in range(1, 16):
            xored ^= nums[i * 16 + j]
        dense_hash.append(xored)

    return ''.join(format(num, '08b') for num in dense_hash)

grid = [knot_hash(f"{key}-{i}") for i in range(128)]

print(sum(row.count('1') for row in grid))