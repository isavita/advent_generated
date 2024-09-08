import operator

def parse_input(filename):
    monkeys = {}
    with open(filename, 'r') as file:
        for line in file:
            name, job = line.strip().split(': ')
            try:
                monkeys[name] = int(job)
            except ValueError:
                monkeys[name] = job.split()
    return monkeys

def evaluate_monkey(name, monkeys, cache):
    if name in cache:
        return cache[name]

    job = monkeys[name]
    if isinstance(job, int):
        result = job
    else:
        left, op, right = job
        left_val = evaluate_monkey(left, monkeys, cache)
        right_val = evaluate_monkey(right, monkeys, cache)
        ops = {'+': operator.add, '-': operator.sub, '*': operator.mul, '/': operator.floordiv}
        result = ops[op](left_val, right_val)

    cache[name] = result
    return result

def solve_monkey_math(filename):
    monkeys = parse_input(filename)
    cache = {}
    root_value = evaluate_monkey('root', monkeys, cache)
    return root_value

print(solve_monkey_math('input.txt'))
