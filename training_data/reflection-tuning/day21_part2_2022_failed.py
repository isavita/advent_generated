from operator import add, sub, mul, truediv

class Monkey:
    def __init__(self, name, job):
        self.name = name
        if job.isdigit():
            self.value = int(job)
            self.operation = None
        else:
            self.value = None
            left, op, right = job.split()
            self.left = left
            self.right = right
            self.operation = {'+': add, '-': sub, '*': mul, '/': truediv}[op]

def parse_input(data):
    monkeys = {}
    for line in data.split('\n'):
        name, job = line.split(': ')
        monkeys[name] = Monkey(name, job)
    return monkeys

def evaluate(monkey, monkeys, humn_value=None):
    if monkey.name == 'humn' and humn_value is not None:
        return humn_value
    if monkey.value is not None:
        return monkey.value
    left_value = evaluate(monkeys[monkey.left], monkeys, humn_value)
    right_value = evaluate(monkeys[monkey.right], monkeys, humn_value)
    return monkey.operation(left_value, right_value)

def solve_part1(monkeys):
    return int(evaluate(monkeys['root'], monkeys))

def solve_part2(monkeys):
    root = monkeys['root']
    left_monkey = monkeys[root.left]
    right_monkey = monkeys[root.right]

    def check_equality(humn_value):
        left_value = evaluate(left_monkey, monkeys, humn_value)
        right_value = evaluate(right_monkey, monkeys, humn_value)
        return left_value - right_value

    # Binary search for the correct humn value
    low, high = 0, 10**15
    while low < high:
        mid = (low + high) // 2
        result = check_equality(mid)
        if result == 0:
            return mid
        elif result < 0:
            low = mid + 1
        else:
            high = mid - 1

    return low

def main(data):
    monkeys = parse_input(data)
    part1 = solve_part1(monkeys)
    part2 = solve_part2(monkeys)
    print(f"Part 1: {part1}")
    print(f"Part 2: {part2}")

# Assuming 'data' contains the puzzle input
# main(data)
