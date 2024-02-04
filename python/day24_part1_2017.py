
from collections import defaultdict

def build_bridge(components, port, strength):
    max_strength = strength
    for i, comp in enumerate(components):
        if port in comp:
            next_port = comp[0] if comp[1] == port else comp[1]
            new_components = components[:i] + components[i+1:]
            max_strength = max(max_strength, build_bridge(new_components, next_port, strength + sum(comp)))
    return max_strength

def main():
    components = []
    with open('input.txt', 'r') as file:
        for line in file:
            ports = list(map(int, line.strip().split('/')))
            components.append(ports)

    strength = build_bridge(components, 0, 0)
    print(strength)

if __name__ == "__main__":
    main()
