
import json

def sum_numbers(data):
    if isinstance(data, int):
        return data
    elif isinstance(data, list):
        return sum(sum_numbers(item) for item in data)
    elif isinstance(data, dict):
        return sum(sum_numbers(value) for value in data.values())
    return 0

with open('input.txt', 'r') as file:
    document = json.load(file)

result = sum_numbers(document)
print(result)
