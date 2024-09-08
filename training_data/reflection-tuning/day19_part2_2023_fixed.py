import re
from collections import defaultdict

def parse_workflows(lines):
    workflows = {}
    for line in lines:
        line = line.strip()
        if not line:
            continue
        match = re.match(r'(\w+)\{(.+)\}', line)
        if match:
            name, rules_str = match.groups()
            rules = []
            for rule in rules_str.split(','):
                if ':' in rule:
                    condition, target = rule.split(':')
                    rules.append((condition, target))
                else:
                    rules.append((None, rule))
            workflows[name] = rules
    return workflows

def parse_parts(lines):
    parts = []
    for line in lines:
        line = line.strip()
        if not line:
            continue
        match = re.match(r'\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}', line)
        if match:
            parts.append(tuple(map(int, match.groups())))
    return parts

def evaluate_condition(part, condition):
    if condition is None:
        return True
    category, op, value = re.match(r'(\w)([<>])(\d+)', condition).groups()
    part_value = part['xmas'.index(category)]
    return eval(f"{part_value} {op} {value}")

def process_part(part, workflows):
    current = 'in'
    while current not in 'AR':
        for condition, target in workflows[current]:
            if evaluate_condition(part, condition):
                current = target
                break
    return current == 'A'

def solve_part1(workflows, parts):
    total = 0
    for part in parts:
        if process_part(part, workflows):
            total += sum(part)
    return total

def count_combinations(workflows, ranges, current='in'):
    if current == 'R':
        return 0
    if current == 'A':
        product = 1
        for low, high in ranges.values():
            product *= max(0, high - low + 1)
        return product

    total = 0
    for condition, target in workflows[current]:
        if condition is None:
            total += count_combinations(workflows, ranges, target)
        else:
            category, op, value = re.match(r'(\w)([<>])(\d+)', condition).groups()
            value = int(value)
            low, high = ranges[category]
            if op == '<':
                true_range = (low, min(value - 1, high))
                false_range = (max(value, low), high)
            else:  # op == '>'
                true_range = (max(value + 1, low), high)
                false_range = (low, min(value, high))

            if true_range[0] <= true_range[1]:
                new_ranges = ranges.copy()
                new_ranges[category] = true_range
                total += count_combinations(workflows, new_ranges, target)

            if false_range[0] <= false_range[1]:
                ranges = ranges.copy()
                ranges[category] = false_range
            else:
                break
    return total

def solve_part2(workflows):
    initial_ranges = {c: (1, 4000) for c in 'xmas'}
    return count_combinations(workflows, initial_ranges)

def main():
    with open('input.txt', 'r') as file:
        content = file.read().split('\n\n')
    
    workflow_lines = content[0].split('\n')
    part_lines = content[1].split('\n')

    workflows = parse_workflows(workflow_lines)
    parts = parse_parts(part_lines)

    print("Part 1:", solve_part1(workflows, parts))
    print("Part 2:", solve_part2(workflows))

if __name__ == "__main__":
    main()
