import re

def parse_input(filename):
    with open(filename, 'r') as file:
        workflows_raw, parts_raw = file.read().strip().split('\n\n')
    
    workflows = {}
    for line in workflows_raw.split('\n'):
        name, rules = re.match(r'(\w+)\{(.+)\}', line).groups()
        workflows[name] = rules.split(',')
    
    parts = []
    for line in parts_raw.split('\n'):
        part = {k: int(v) for k, v in re.findall(r'(\w)=(\d+)', line)}
        parts.append(part)
    
    return workflows, parts

def process_part(part, workflows):
    current = 'in'
    while current not in 'AR':
        for rule in workflows[current]:
            if ':' in rule:
                condition, next_workflow = rule.split(':')
                if eval(condition, {}, part):
                    current = next_workflow
                    break
            else:
                current = rule
    return current == 'A'

def solve(filename):
    workflows, parts = parse_input(filename)
    return sum(sum(part.values()) for part in parts if process_part(part, workflows))

print(solve('input.txt'))
