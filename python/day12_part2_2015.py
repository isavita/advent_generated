import json

def sum_numbers(data):
    if isinstance(data, int):
        return data
    if isinstance(data, list):
        return sum(sum_numbers(d) for d in data)
    if isinstance(data, dict):
        if "red" in data.values():
            return 0
        return sum(sum_numbers(v) for v in data.values())
    return 0

with open("input.txt", "r") as file:
    data = json.load(file)

print(sum_numbers(data))