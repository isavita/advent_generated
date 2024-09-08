import re
from collections import deque

def parse_workflows(lines):
    workflows = {}
    for line in lines:
        if not line:
            break
        name, rules = re.match(r'(\w+)\{(.+)\}', line).groups()
        workflows[name] = rules.split(',')
    return workflows

def process_ranges(workflows):
    def dfs(workflow, ranges):
        if workflow == 'A':
            return (ranges['x'][1] - ranges['x'][0] + 1) * \
                   (ranges['m'][1] - ranges['m'][0] + 1) * \
                   (ranges['a'][1] - ranges['a'][0] + 1) * \
                   (ranges['s'][1] - ranges['s'][0] + 1)
        if workflow == 'R':
            return 0

        total = 0
        for rule in workflows[workflow]:
            if ':' in rule:
                condition, next_workflow = rule.split(':')
                rating = condition[0]
                op = condition[1]
                value = int(condition[2:])

                if op == '<':
                    if ranges[rating][0] < value:
                        new_ranges = ranges.copy()
                        new_ranges[rating] = (ranges[rating][0], min(value - 1, ranges[rating][1]))
                        total += dfs(next_workflow, new_ranges)
                        ranges[rating] = (value, ranges[rating][1])
                    if ranges[rating][0] > ranges[rating][1]:
                        break
                else:  # '>'
                    if ranges[rating][1] > value:
                        new_ranges = ranges.copy()
                        new_ranges[rating] = (max(value + 1, ranges[rating][0]), ranges[rating][1])
                        total += dfs(next_workflow, new_ranges)
                        ranges[rating] = (ranges[rating][0], value)
                    if ranges[rating][0] > ranges[rating][1]:
                        break
            else:
                total += dfs(rule, ranges)
                break
        return total

    initial_ranges = {rating: (1, 4000) for rating in 'xmas'}
    return dfs('in', initial_ranges)

with open('input.txt', 'r') as file:
    workflows = parse_workflows(file)

result = process_ranges(workflows)
print(result)
