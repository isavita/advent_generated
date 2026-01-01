import sys

def main():
    try:
        with open("input.txt") as f:
            lines = f.readlines()
    except FileNotFoundError:
        return
    ranges = []
    for raw in lines:
        line = raw.strip()
        if not line:
            continue
        parts = line.split("-")
        if len(parts) != 2:
            continue
        a, b = int(parts[0].strip()), int(parts[1].strip())
        if a > b:
            a, b = b, a
        ranges.append((a, b))
    if not ranges:
        print("Total fresh IDs: 0")
        return
    ranges.sort()
    total = 0
    cur_min, cur_max = ranges[0]
    for mn, mx in ranges[1:]:
        if mn <= cur_max:
            if mx > cur_max:
                cur_max = mx
        else:
            total += cur_max - cur_min + 1
            cur_min, cur_max = mn, mx
    total += cur_max - cur_min + 1
    print(f"Total fresh IDs: {total}")

if __name__ == "__main__":
    main()
