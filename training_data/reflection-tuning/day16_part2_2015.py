def parse_input(filename):
    aunts = {}
    with open(filename, 'r') as file:
        for line in file:
            parts = line.strip().split(': ', 1)
            aunt_number = int(parts[0].split()[1])
            attributes = {k: int(v) for k, v in (item.split(': ') for item in parts[1].split(', '))}
            aunts[aunt_number] = attributes
    return aunts

def check_match(aunt, mfcsam, part2=False):
    for key, value in aunt.items():
        if key not in mfcsam:
            continue
        if part2 and key in ['cats', 'trees']:
            if value <= mfcsam[key]:
                return False
        elif part2 and key in ['pomeranians', 'goldfish']:
            if value >= mfcsam[key]:
                return False
        elif value != mfcsam[key]:
            return False
    return True

def find_aunt_sue(aunts, mfcsam):
    part1_match = part2_match = None
    for number, attributes in aunts.items():
        if part1_match is None and check_match(attributes, mfcsam):
            part1_match = number
        if part2_match is None and check_match(attributes, mfcsam, part2=True):
            part2_match = number
        if part1_match is not None and part2_match is not None:
            break
    return part1_match, part2_match

mfcsam = {
    'children': 3, 'cats': 7, 'samoyeds': 2, 'pomeranians': 3,
    'akitas': 0, 'vizslas': 0, 'goldfish': 5, 'trees': 3,
    'cars': 2, 'perfumes': 1
}

aunts = parse_input('input.txt')
part1, part2 = find_aunt_sue(aunts, mfcsam)

print(f"Part 1: The Aunt Sue who got you the gift is number {part1}")
print(f"Part 2: The real Aunt Sue is number {part2}")
