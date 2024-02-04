with open('input.txt', 'r') as file:
    components = [list(map(int, line.strip().split('/'))) for line in file]

def build_bridge(components, current_port, strength, length):
    possible_bridges = []
    for i, component in enumerate(components):
        if current_port in component:
            next_port = component[0] if component[1] == current_port else component[1]
            new_components = components[:i] + components[i+1:]
            possible_bridges.extend(build_bridge(new_components, next_port, strength + sum(component), length + 1))
    if not possible_bridges:
        return [(strength, length)]
    return possible_bridges

all_bridges = build_bridge(components, 0, 0, 0)
max_length = max([bridge[1] for bridge in all_bridges])
max_strength = max([bridge[0] for bridge in all_bridges if bridge[1] == max_length])

print(max_strength)