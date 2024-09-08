def parse_input(filename):
    components = []
    with open(filename, 'r') as file:
        for line in file:
            a, b = map(int, line.strip().split('/'))
            components.append((a, b))
    return components

def dfs(components, port, used, strength):
    max_strength = strength
    for i, (a, b) in enumerate(components):
        if i not in used and (a == port or b == port):
            new_port = b if a == port else a
            new_strength = strength + a + b
            used.add(i)
            max_strength = max(max_strength, dfs(components, new_port, used, new_strength))
            used.remove(i)
    return max_strength

def find_strongest_bridge(components):
    return max(dfs(components, 0, set(), 0) for component in components if 0 in component)

components = parse_input("input.txt")
print(find_strongest_bridge(components))
