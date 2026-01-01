import sys, os

def main():
    data = open("input.txt").read()
    cleaned = ''.join(',' if c.isspace() else c for c in data)
    tokens = [t for t in cleaned.split(',') if t]

    pow10 = [1]
    for _ in range(20):
        pow10.append(pow10[-1] * 10)

    ids = set()
    for token in tokens:
        if '-' not in token:
            continue
        a, b = token.split('-', 1)
        lo, hi = (int(a), int(b)) if int(a) <= int(b) else (int(b), int(a))
        for k in range(1, 11):
            mul = pow10[k] + 1
            min_seed = pow10[k - 1]
            max_seed = pow10[k] - 1
            s_min = (lo + mul - 1) // mul
            s_max = hi // mul
            if s_min < min_seed:
                s_min = min_seed
            if s_max > max_seed:
                s_max = max_seed
            if s_min > s_max:
                continue
            for seed in range(s_min, s_max + 1):
                ids.add(seed * mul)

    print(sum(ids))

if __name__ == "__main__":
    main()
