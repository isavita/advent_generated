from collections import defaultdict

def parse_input(input_str):
    components = [tuple(map(int, line.split('/'))) for line in input_str.strip().split('\n')]
    return components

def build_bridges(components):
    port_map = defaultdict(list)
    for a, b in components:
        port_map[a].append(b)
        port_map[b].append(a)
    
    def dfs(port, used, strength, length):
        max_strength = strength
        max_length = length
        longest_strongest = (length, strength)

        for next_port in port_map[port]:
            component = tuple(sorted((port, next_port)))
            if component not in used:
                new_strength = strength + port + next_port
                new_length = length + 1
                new_used = used | {component}
                s, l, ls = dfs(next_port, new_used, new_strength, new_length)
                max_strength = max(max_strength, s)
                if l > max_length or (l == max_length and ls[1] > longest_strongest[1]):
                    max_length = l
                    longest_strongest = ls

        return max_strength, max_length, longest_strongest

    return dfs(0, set(), 0, 0)

def solve(input_str):
    components = parse_input(input_str)
    max_strength, _, (_, strongest_longest) = build_bridges(components)
    return max_strength, strongest_longest

# Example usage
input_str = """0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10"""

part1, part2 = solve(input_str)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
