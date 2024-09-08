def merge_ranges(ranges):
    ranges.sort(key=lambda x: x[0])
    merged = []
    for start, end in ranges:
        if not merged or start > merged[-1][1] + 1:
            merged.append([start, end])
        else:
            merged[-1][1] = max(merged[-1][1], end)
    return merged

def find_lowest_unblocked_ip(ranges):
    merged = merge_ranges(ranges)
    if merged[0][0] > 0:
        return 0
    for i in range(len(merged) - 1):
        if merged[i][1] + 1 < merged[i+1][0]:
            return merged[i][1] + 1
    return merged[-1][1] + 1

# Read input from file
with open('input.txt', 'r') as file:
    ranges = [list(map(int, line.strip().split('-'))) for line in file]

# Find and print the lowest unblocked IP
print(find_lowest_unblocked_ip(ranges))
