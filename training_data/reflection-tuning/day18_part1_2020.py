def evaluate_simple(expr):
    tokens = expr.replace(' ', '').replace('(', '').replace(')', '').replace('*', ' * ').replace('+', ' + ').split()
    result = int(tokens[0])
    for i in range(1, len(tokens), 2):
        if tokens[i] == '+':
            result += int(tokens[i+1])
        else:  # '*'
            result *= int(tokens[i+1])
    return result

def evaluate_expression(expr):
    stack = []
    current = ''
    for char in expr:
        if char == '(':
            stack.append(current)
            current = ''
        elif char == ')':
            value = evaluate_simple(current)
            current = stack.pop() + str(value)
        else:
            current += char
    return evaluate_simple(current)

total = 0
with open('input.txt', 'r') as file:
    for line in file:
        total += evaluate_expression(line.strip())

print(total)
