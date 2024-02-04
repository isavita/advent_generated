
def read_input(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()
        template = lines[0].strip()
        rules = {}
        for line in lines[1:]:
            if line.strip() == "":
                continue
            parts = line.strip().split(" -> ")
            rules[parts[0]] = parts[1]
    return template, rules

template, rules = read_input("input.txt")
pair_counts = {}
for i in range(len(template)-1):
    pair = template[i:i+2]
    if pair in pair_counts:
        pair_counts[pair] += 1
    else:
        pair_counts[pair] = 1

for step in range(40):
    new_pair_counts = {}
    for pair, count in pair_counts.items():
        if pair in rules:
            insert = rules[pair]
            new_pair_counts[pair[0]+insert] = new_pair_counts.get(pair[0]+insert, 0) + count
            new_pair_counts[insert+pair[1]] = new_pair_counts.get(insert+pair[1], 0) + count
        else:
            new_pair_counts[pair] = new_pair_counts.get(pair, 0) + count
    pair_counts = new_pair_counts

element_counts = {}
for pair, count in pair_counts.items():
    if pair[0] in element_counts:
        element_counts[pair[0]] += count
    else:
        element_counts[pair[0]] = count
element_counts[template[-1]] = element_counts.get(template[-1], 0) + 1

max_count = 0
min_count = float('inf')
for count in element_counts.values():
    max_count = max(max_count, count)
    min_count = min(min_count, count)

print(max_count - min_count)
