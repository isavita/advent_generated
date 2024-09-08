import re

def parse_input(input_text):
    rules_text, messages = input_text.strip().split('\n\n')
    rules = {}
    for rule in rules_text.split('\n'):
        num, content = rule.split(': ')
        rules[int(num)] = content.strip('"').split(' ')
    return rules, messages.split('\n')

def match_rule(rules, rule_num, message, start=0):
    if start == len(message):
        return []
    
    rule = rules[rule_num]
    if len(rule) == 1 and rule[0] in 'ab':
        if message[start] == rule[0]:
            return [start + 1]
        return []
    
    matches = []
    for subrule in ' '.join(rule).split(' | '):
        subrule_nums = [int(x) for x in subrule.split()]
        sub_matches = [start]
        for sub_num in subrule_nums:
            new_matches = []
            for pos in sub_matches:
                new_matches.extend(match_rule(rules, sub_num, message, pos))
            sub_matches = new_matches
            if not sub_matches:
                break
        matches.extend(sub_matches)
    
    return matches

def count_matching_messages(rules, messages):
    count = 0
    for message in messages:
        if len(message) in match_rule(rules, 0, message):
            count += 1
    return count

def solve(input_text):
    rules, messages = parse_input(input_text)
    return count_matching_messages(rules, messages)

# Example usage:
input_text = """0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb"""

result = solve(input_text)
print(result)  # This should print 2
