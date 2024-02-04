
import re

def some_assembly_required(input):
    wire_to_rule = {}

    for inst in input.split("\n"):
        parts = inst.split(" -> ")
        wire_to_rule[parts[1]] = parts[0]

    a_signal = memo_dfs(wire_to_rule, "a", {})

    wire_to_rule["b"] = str(a_signal)
    return memo_dfs(wire_to_rule, "a", {})

def memo_dfs(graph, entry, memo):
    if entry in memo:
        return memo[entry]

    if re.match("[0-9]", entry):
        return int(entry)

    source_rule = graph[entry]
    parts = source_rule.split(" ")

    result = 0
    if len(parts) == 1:
        result = memo_dfs(graph, parts[0], memo)
    elif parts[0] == "NOT":
        start = memo_dfs(graph, parts[1], memo)
        result = (65535) ^ start
    elif parts[1] == "AND":
        result = memo_dfs(graph, parts[0], memo) & memo_dfs(graph, parts[2], memo)
    elif parts[1] == "OR":
        result = memo_dfs(graph, parts[0], memo) | memo_dfs(graph, parts[2], memo)
    elif parts[1] == "LSHIFT":
        result = memo_dfs(graph, parts[0], memo) << memo_dfs(graph, parts[2], memo)
    elif parts[1] == "RSHIFT":
        result = memo_dfs(graph, parts[0], memo) >> memo_dfs(graph, parts[2], memo)

    memo[entry] = result
    return result

with open("input.txt", "r") as file:
    input_data = file.read().strip()
    print(some_assembly_required(input_data))
