
import re

def evaluate_simple(expression):
    parts = re.findall(r'\d+|\+|\*', expression)
    total = int(parts[0])
    for i in range(1, len(parts), 2):
        if parts[i] == '+':
            total += int(parts[i + 1])
        else:
            total *= int(parts[i + 1])
    return total

def evaluate_advanced(expression):
    parts = re.findall(r'\d+|\+|\*', expression)
    while '+' in parts:
        i = parts.index('+')
        total = int(parts[i - 1]) + int(parts[i + 1])
        parts = parts[:i - 1] + [str(total)] + parts[i + 2:]
    total = int(parts[0])
    for i in range(1, len(parts), 2):
        total *= int(parts[i + 1])
    return total

def evaluate_expression(expression, evaluate_fn):
    while '(' in expression:
        start = expression.rfind('(')
        end = start + expression[start:].find(')')
        expression = expression[:start] + str(evaluate_fn(expression[start + 1: end])) + expression[end + 1:]
    return evaluate_fn(expression)

with open('input.txt', 'r') as file:
    expressions = file.read().strip().split('\n')

result_part1 = sum([evaluate_expression(expression, evaluate_simple) for expression in expressions])
result_part2 = sum([evaluate_expression(expression, evaluate_advanced) for expression in expressions])

print(result_part1)
print(result_part2)
