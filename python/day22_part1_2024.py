
def next_secret(s: int) -> int:
    x = s * 64
    s ^= x
    s &= 0xFFFFFF
    x = s // 32
    s ^= x
    s &= 0xFFFFFF
    x = s * 2048
    s ^= x
    s &= 0xFFFFFF
    return s

with open("input.txt", "r") as f:
    buyers = [int(line) for line in f if line.strip()]

total = 0
for b in buyers:
    s = b
    for _ in range(2000):
        s = next_secret(s)
    total += s

print(total)
