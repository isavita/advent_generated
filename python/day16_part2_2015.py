
with open('input.txt') as f:
    lines = f.readlines()

mfcsam = {
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

for line in lines:
    sue_num = int(line.split()[1][:-1])
    items = line.split()[2:]
    valid = True
    for i in range(0, len(items), 2):
        prop = items[i][:-1]
        val = int(items[i+1].replace(',', ''))
        if prop in ['cats', 'trees']:
            if mfcsam[prop] >= val:
                valid = False
                break
        elif prop in ['pomeranians', 'goldfish']:
            if mfcsam[prop] <= val:
                valid = False
                break
        else:
            if mfcsam[prop] != val:
                valid = False
                break
    if valid:
        print(sue_num)
