import re

# MFCSAM results
mfcsam = {
    'children': 3, 'cats': 7, 'samoyeds': 2, 'pomeranians': 3,
    'akitas': 0, 'vizslas': 0, 'goldfish': 5, 'trees': 3,
    'cars': 2, 'perfumes': 1
}

# Read input and find matching Aunt Sue
with open('input.txt', 'r') as file:
    for line in file:
        sue_num = int(re.search(r'Sue (\d+):', line).group(1))
        attributes = re.findall(r'(\w+): (\d+)', line)
        
        if all(mfcsam[attr] == int(value) for attr, value in attributes):
            print(sue_num)
            break

