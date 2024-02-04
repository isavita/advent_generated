with open('input.txt') as f:
    lines = f.read().splitlines()

def evaluate(expression):
    while '(' in expression:
        start = expression.rfind('(')
        end = start + expression[start:].index(')')
        expression = expression[:start] + str(evaluate(expression[start+1:end])) + expression[end+1:]

    tokens = expression.split()
    total = int(tokens[0])
    index = 1
    while index < len(tokens):
        if tokens[index] == '+':
            total += int(tokens[index+1])
        elif tokens[index] == '*':
            total *= int(tokens[index+1])
        index += 2
    return total

result = sum([evaluate(line) for line in lines])
print(result)