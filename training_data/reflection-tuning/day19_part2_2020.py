import re

def parse_rules(rules_input):
    rules = {}
    for line in rules_input.split('\n'):
        num, rule = line.split(': ')
        rules[int(num)] = rule.strip('"')
    return rules

def generate_regex(rules, rule_num, part2=False, depth=0):
    if depth > 20:  # Prevent infinite recursion
        return ''
    
    if rule_num not in rules:
        return ''

    rule = rules[rule_num]
    
    if rule.isalpha():
        return rule
    
    if part2:
        if rule_num == 8:
            return f"({generate_regex(rules, 42, part2, depth+1)}+)"
        elif rule_num == 11:
            r42 = generate_regex(rules, 42, part2, depth+1)
            r31 = generate_regex(rules, 31, part2, depth+1)
            return f"({r42}({r42}({r42}({r42}{r31})?{r31})?{r31})?{r31})"

    parts = []
    for subrule in rule.split('|'):
        subparts = []
        for num in subrule.split():
            subparts.append(generate_regex(rules, int(num), part2, depth+1))
        parts.append(''.join(subparts))
    
    return f"({'|'.join(parts)})"

def solve(filename):
    with open(filename, 'r') as file:
        rules_input, messages = file.read().strip().split('\n\n')
    
    rules = parse_rules(rules_input)
    
    regex_part1 = re.compile(f"^{generate_regex(rules, 0)}$")
    part1 = sum(1 for msg in messages.split('\n') if regex_part1.match(msg))
    
    regex_part2 = re.compile(f"^{generate_regex(rules, 0, True)}$")
    part2 = sum(1 for msg in messages.split('\n') if regex_part2.match(msg))
    
    return part1, part2

# Example usage
part1, part2 = solve("input.txt")
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
