import re

def parse_input(filename):
    with open(filename, 'r') as f:
        rules_raw, messages = f.read().strip().split('\n\n')
    
    rules = {}
    for rule in rules_raw.split('\n'):
        num, content = rule.split(': ')
        rules[num] = content.strip('"')
    
    return rules, messages.split('\n')

def generate_regex(rules, rule_num, part2=False):
    if part2 and rule_num == '8':
        return f"({generate_regex(rules, '42', part2)})+"
    if part2 and rule_num == '11':
        return f"(?P<r11>{generate_regex(rules, '42', part2)}(?P=r11)?{generate_regex(rules, '31', part2)})"
    
    rule = rules[rule_num]
    if rule in 'ab':
        return rule
    if '|' in rule:
        return f"({'|'.join(generate_regex(rules, r.strip(), part2) for r in rule.split('|'))})"
    return ''.join(generate_regex(rules, r, part2) for r in rule.split())

def match_part2(regex42, regex31, message):
    count42 = count31 = 0
    while message:
        if re.match(regex42, message):
            count42 += 1
            message = message[8:]  # Assuming fixed length of 8 for simplicity
        elif count42 > count31 > 0 and re.match(regex31, message):
            count31 += 1
            message = message[8:]
        else:
            return False
    return count42 > count31 > 0

def solve(filename):
    rules, messages = parse_input(filename)
    
    # Part 1
    regex = re.compile(f"^{generate_regex(rules, '0')}$")
    part1 = sum(1 for msg in messages if regex.match(msg))
    
    # Part 2
    regex42 = re.compile(f"^{generate_regex(rules, '42', True)}$")
    regex31 = re.compile(f"^{generate_regex(rules, '31', True)}$")
    part2 = sum(1 for msg in messages if match_part2(regex42, regex31, msg))
    
    return part1, part2

part1, part2 = solve("input.txt")
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
