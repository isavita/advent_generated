def merge_ranges(ranges):
    ranges.sort(key=lambda x: x[0])
    merged = []
    for start, end in ranges:
        if not merged or start > merged[-1][1] + 1:
            merged.append([start, end])
        else:
            merged[-1][1] = max(merged[-1][1], end)
    return merged

def solve():
    with open('input.txt', 'r') as file:
        ranges = [tuple(map(int, line.strip().split('-'))) for line in file]

    merged = merge_ranges(ranges)

    # Part 1: Lowest unblocked IP
    lowest_unblocked = 0 if merged[0][0] > 0 else merged[0][1] + 1
    print(f"Part 1: {lowest_unblocked}")

    # Part 2: Number of allowed IPs
    blocked_count = sum(end - start + 1 for start, end in merged)
    allowed_count = 4294967296 - blocked_count
    print(f"Part 2: {allowed_count}")

solve()
