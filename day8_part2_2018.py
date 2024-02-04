with open("input.txt") as f:
    data = list(map(int, f.read().split()))

def parse_node(data):
    num_children = data[0]
    num_metadata = data[1]
    data = data[2:]
    total = 0
    values = []

    for _ in range(num_children):
        child_total, child_value, data = parse_node(data)
        total += child_total
        values.append(child_value)

    total += sum(data[:num_metadata])

    if num_children == 0:
        return total, sum(data[:num_metadata]), data[num_metadata:]
    else:
        value = sum(values[i - 1] for i in data[:num_metadata] if i > 0 and i <= len(values))
        return total, value, data[num_metadata:]

total, root_value, _ = parse_node(data)
print(total)
print(root_value)