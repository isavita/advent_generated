
import sys

class Row:
    def __init__(self, springs, group):
        self.springs = springs
        self.group = group

def parse_input(input_lines):
    rows = []
    for line in input_lines:
        parts = line.split(" ")
        springs = parts[0]
        group = list(map(int, parts[1].split(",")))
        rows.append(Row(springs, group))
    return rows

def count_arrangements_recursive(row, i_springs, i_group, i_contiguous_damaged, cache):
    if i_springs == len(row.springs):
        if i_group == len(row.group) and i_contiguous_damaged == 0:
            return 1
        elif i_group == len(row.group) - 1 and i_contiguous_damaged == row.group[i_group]:
            return 1
        return 0

    cache_key = (i_springs, i_group, i_contiguous_damaged)
    if cache_key in cache:
        return cache[cache_key]

    res = 0
    char = row.springs[i_springs]
    if char in ['.', '?']:
        if i_contiguous_damaged == 0:
            res += count_arrangements_recursive(row, i_springs + 1, i_group, i_contiguous_damaged, cache)
        elif i_contiguous_damaged == row.group[i_group]:
            res += count_arrangements_recursive(row, i_springs + 1, i_group + 1, 0, cache)
    if char in ['#', '?']:
        if i_group < len(row.group) and i_contiguous_damaged < row.group[i_group]:
            res += count_arrangements_recursive(row, i_springs + 1, i_group, i_contiguous_damaged + 1, cache)

    cache[cache_key] = res
    return res

def count_arrangements(row):
    return count_arrangements_recursive(row, 0, 0, 0, {})

def unfold_row(row, unfolding_factor):
    new_row = Row(row.springs, row.group.copy())
    for _ in range(1, unfolding_factor):
        new_row.springs += "?" + row.springs
        new_row.group.extend(row.group)
    return new_row

def solve(input_lines):
    rows = parse_input(input_lines)
    unfolded_rows = [unfold_row(row, 5) for row in rows]
    return sum(count_arrangements(row) for row in unfolded_rows)

def read_file(file_name):
    with open(file_name, 'r') as file:
        return [line.strip() for line in file.readlines()]

def main():
    input_lines = read_file("input.txt")
    print(solve(input_lines))

if __name__ == "__main__":
    main()
