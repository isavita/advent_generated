import sys
import threading
import math

def main():
    import sys
    import re

    sys.setrecursionlimit(1000000)

    workflows = {}
    variables = ['x', 'm', 'a', 's']
    variable_indices = {'x': 0, 'm': 1, 'a':2, 's':3}

    lines = []
    with open('input.txt', 'r') as f:
        lines = f.readlines()

    # Parse workflows
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if line == '':
            break
        match = re.match(r'(\w+)\{(.+)\}', line)
        if match:
            name = match.group(1)
            rules_str = match.group(2)
            rules = []
            # Split rules by commas, but be careful with conditions that contain commas
            rules_parts = []
            depth = 0
            current_rule = ''
            for c in rules_str:
                if c == ',' and depth == 0:
                    rules_parts.append(current_rule)
                    current_rule = ''
                else:
                    if c == '(':
                        depth +=1
                    elif c == ')':
                        depth -=1
                    current_rule += c
            if current_rule:
                rules_parts.append(current_rule)
            for rule_str in rules_parts:
                if ':' in rule_str:
                    condition, destination = rule_str.split(':',1)
                    rules.append({'condition': condition, 'destination': destination})
                else:
                    rules.append({'condition': None, 'destination': rule_str})
            workflows[name] = rules
        i +=1

    # Skip blank lines
    while i < len(lines) and lines[i].strip() == '':
        i +=1

    # Parse parts (not needed for part two)
    # parts = []
    # while i < len(lines):
    #     line = lines[i].strip()
    #     match = re.match(r'\{(.+)\}', line)
    #     if match:
    #         part_str = match.group(1)
    #         part = {}
    #         for item in part_str.split(','):
    #             k,v = item.strip().split('=')
    #             part[k.strip()] = int(v.strip())
    #         parts.append(part)
    #     i +=1

    memo = {}

    def process(workflow_name, constraints):
        key = (workflow_name, constraints)
        if key in memo:
            return memo[key]

        x_range, m_range, a_range, s_range = constraints

        # If any constraints are invalid, return 0
        if x_range[0] > x_range[1] or m_range[0] > m_range[1] or a_range[0] > a_range[1] or s_range[0] > s_range[1]:
            return 0

        # If the ranges are empty (no integer values), return 0
        if x_range[1] < x_range[0] or m_range[1] < m_range[0] or a_range[1] < a_range[0] or s_range[1] < s_range[0]:
            return 0

        # Get the workflow rules
        rules = workflows[workflow_name]

        result = 0

        def count_constraints(x_range, m_range, a_range, s_range):
            # Number of integer points in the ranges
            x_count = x_range[1] - x_range[0] +1
            m_count = m_range[1] - m_range[0] +1
            a_count = a_range[1] - a_range[0] +1
            s_count = s_range[1] - s_range[0] +1
            total = x_count * m_count * a_count * s_count
            if total <0:
                return 0
            return total

        i = 0
        while i < len(rules):
            rule = rules[i]
            condition = rule['condition']
            destination = rule['destination']

            if condition:
                # Parse the condition
                match = re.match(r'([xmas])([<>])(\d+)', condition)
                if not match:
                    print(f"Invalid condition: {condition}")
                    sys.exit(1)
                var = match.group(1)
                op = match.group(2)
                value = int(match.group(3))

                var_idx = variable_indices[var]
                var_range = constraints[var_idx]

                # Split the variable's range into true and false parts
                if op == '>':
                    true_range = (max(var_range[0], value+1), var_range[1])
                    false_range = (var_range[0], min(var_range[1], value))
                elif op == '<':
                    true_range = (var_range[0], min(var_range[1], value-1))
                    false_range = (max(var_range[0], value), var_range[1])
                else:
                    print(f"Invalid operator: {op}")
                    sys.exit(1)

                # First, process the true branch (condition is satisfied)
                if true_range[0] <= true_range[1]:
                    new_constraints = list(constraints)
                    new_constraints[var_idx] = true_range
                    new_constraints = tuple(new_constraints)
                    if destination == 'A':
                        result += count_constraints(*new_constraints)
                    elif destination == 'R':
                        pass
                    else:
                        result += process(destination, new_constraints)
                # Then, process the false branch (condition is not satisfied), proceed to next rule
                if false_range[0] <= false_range[1]:
                    # Update the variable's range for the false branch
                    constraints = list(constraints)
                    constraints[var_idx] = false_range
                    constraints = tuple(constraints)
                    i +=1
                    continue
                else:
                    # No valid values in false branch, stop processing
                    break
            else:
                # No condition, always applies
                if destination == 'A':
                    result += count_constraints(*constraints)
                elif destination == 'R':
                    pass
                else:
                    result += process(destination, constraints)
                break  # Since this rule always applies, stop processing further rules
                # i +=1
        memo[key] = result
        return result

    # Initial constraints: all variables in [1,4000]
    initial_constraints = ((1,4000),(1,4000),(1,4000),(1,4000))

    total = process('in', initial_constraints)

    print(total)

threading.Thread(target=main).start()
