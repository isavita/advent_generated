import re

def evaluate_part1(expr):
    expr = expr.replace(' ', '')
    i, result, op = 0, 0, '+'
    while i < len(expr):
        if expr[i] in '0123456789':
            num = 0
            while i < len(expr) and expr[i] in '0123456789':
                num = num * 10 + int(expr[i])
                i += 1
            if op == '+':
                result += num
            else:
                result *= num
        elif expr[i] in '+*':
            op = expr[i]
            i += 1
        elif expr[i] == '(':
            paren_count, start = 1, i + 1
            while paren_count > 0:
                i += 1
                if expr[i] == '(':
                    paren_count += 1
                elif expr[i] == ')':
                    paren_count -= 1
            sub_result = evaluate_part1(expr[start:i])
            if op == '+':
                result += sub_result
            else:
                result *= sub_result
            i += 1
    return result

def evaluate_part2(expr):
    while '(' in expr:
        expr = re.sub(r'\(([^()]+)\)', lambda m: str(evaluate_part2(m.group(1))), expr)
    
    # Evaluate additions
    while '+' in expr:
        expr = re.sub(r'(\d+)\s*\+\s*(\d+)', lambda m: str(int(m.group(1)) + int(m.group(2))), expr)
    
    # Evaluate multiplications
    return eval(expr.replace('*', '*'))

with open('input.txt', 'r') as file:
    expressions = file.readlines()

part1_sum = sum(evaluate_part1(expr.strip()) for expr in expressions)
part2_sum = sum(evaluate_part2(expr.strip()) for expr in expressions)

print(f"Part 1 answer: {part1_sum}")
print(f"Part 2 answer: {part2_sum}")
