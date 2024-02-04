
import re

def parse_input(file_name):
    with open(file_name, 'r') as file:
        data = file.read()
    return data

def check_sue(data):
    mfcsam_data = {
        'children': 3,
        'cats': 7,
        'samoyeds': 2,
        'pomeranians': 3,
        'akitas': 0,
        'vizslas': 0,
        'goldfish': 5,
        'trees': 3,
        'cars': 2,
        'perfumes': 1
    }
    for line in data.split('\n'):
        sue_num = int(re.search(r'\d+', line).group())
        attributes = re.findall(r'(\w+): (\d+)', line)
        sue_data = {attr: int(val) for attr, val in attributes}
        if all(mfcsam_data[attr] == val for attr, val in sue_data.items()):
            return sue_num

data = parse_input("input.txt")
result = check_sue(data)
print(result)
