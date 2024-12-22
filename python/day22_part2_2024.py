
def next_secret(s: int) -> int:
    mod = 1 << 24
    x = s * 64
    s ^= x
    s &= mod - 1
    x = s // 32
    s ^= x
    s &= mod - 1
    x = s * 2048
    s ^= x
    s &= mod - 1
    return s

def encode_change4(c1: int, c2: int, c3: int, c4: int) -> int:
    return (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19

def solve():
    num_steps = 2000
    pattern_count = 19**4
    
    with open("input.txt", "r") as f:
        initials = [int(line) for line in f if line.strip()]

    buyers = []
    for init_val in initials:
        prices = []
        s = init_val
        for _ in range(num_steps + 1):
            prices.append(s % 10)
            s = next_secret(s)
        changes = [prices[j + 1] - prices[j] for j in range(num_steps)]
        buyers.append((prices, changes))

    global_sum = [0] * pattern_count

    for prices, changes in buyers:
        local_price = [-1] * pattern_count
        for i in range(num_steps - 3):
            c1, c2, c3, c4 = changes[i], changes[i+1], changes[i+2], changes[i+3]
            
            if not all(-9 <= c <= 9 for c in (c1,c2,c3,c4)):
                continue
            
            idx = encode_change4(c1, c2, c3, c4)
            if local_price[idx] < 0:
                local_price[idx] = prices[i + 4]
        for idx, p in enumerate(local_price):
            if p >= 0:
                global_sum[idx] += p

    print(max(global_sum))

solve()
