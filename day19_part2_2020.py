
import re

def main():
    with open("input.txt", "r") as file:
        input_data = file.read().strip()
    result = solve(input_data)
    print(result)

def solve(input_data):
    graph, messages = parse_input(input_data)

    fill_in_graph(graph, 42)
    fill_in_graph(graph, 31)

    part42 = f"({'|'.join(graph[42].resolved)})"
    part31 = f"({'|'.join(graph[31].resolved)})"

    rule8_string = f"({part42})+"

    def make_regexp(num):
        return re.compile(f"^{rule8_string}{part42}{{{num}}}{part31}{{{num}}}$")

    match_rule_zero = 0
    for m in messages:
        for i in range(1, 10):
            pattern = make_regexp(i)
            if pattern.match(m):
                match_rule_zero += 1
                break

    return match_rule_zero

def fill_in_graph(graph, entry):
    if graph[entry].resolved:
        return graph[entry].resolved.copy()

    for option in graph[entry].options:
        resolved = [""]
        for entry_point in option:
            nested_resolve_vals = fill_in_graph(graph, entry_point)
            new_resolved = []
            for next_piece in nested_resolve_vals:
                for prev in resolved:
                    new_resolved.append(prev + next_piece)
            resolved = new_resolved
        graph[entry].resolved.extend(resolved)

    return graph[entry].resolved

class Rule:
    def __init__(self):
        self.resolved = []
        self.options = []

def parse_input(input_data):
    parts = input_data.split("\n\n")

    rules = {}
    for r in parts[0].split("\n"):
        if re.search("[a-z]", r):
            num, char = re.match(r"(\d+): \"(\w)\"", r).groups()
            rules[int(num)] = Rule()
            rules[int(num)].resolved = [char]
        else:
            key, rule_nums = r.split(": ")
            key = int(key)
            new_rule = Rule()
            for option in rule_nums.split(" | "):
                nums = [int(n) for n in option.split()]
                new_rule.options.append(nums)
            rules[key] = new_rule

    messages = parts[1].split("\n")

    return rules, messages

if __name__ == "__main__":
    main()
