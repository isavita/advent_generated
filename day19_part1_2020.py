with open("input.txt") as file:
    data = file.read()

rules, messages = data.split("\n\n")

rules_dict = {}
for rule in rules.split("\n"):
    key, value = rule.split(": ")
    rules_dict[key] = value

def get_rule_pattern(rule_num):
    rule = rules_dict[rule_num]
    if rule.startswith('"'):
        return rule[1]
    else:
        result = []
        for part in rule.split(" | "):
            result.append("".join(get_rule_pattern(num) for num in part.split()))
        return "(" + "|".join(result) + ")"

pattern = "^" + get_rule_pattern("0") + "$"

import re
count = 0
for message in messages.split("\n"):
    if re.match(pattern, message):
        count += 1

print(count)