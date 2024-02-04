
def react_polymer(polymer):
    stack = []
    for unit in polymer:
        if stack and unit.swapcase() == stack[-1]:
            stack.pop()
        else:
            stack.append(unit)
    return len(stack)

with open('input.txt', 'r') as file:
    polymer = file.read().strip()

result = react_polymer(polymer)
print(result)
