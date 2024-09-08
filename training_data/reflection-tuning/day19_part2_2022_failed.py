import re
from collections import deque

def parse_blueprint(line):
    numbers = list(map(int, re.findall(r'\d+', line)))
    return (numbers[0], numbers[1], numbers[2], numbers[3], numbers[4], numbers[5], numbers[6])

def solve(blueprints, time):
    def dfs(blueprint, time, ore, clay, obsidian, geodes, ore_robots, clay_robots, obsidian_robots, geode_robots):
        if time == 0:
            return geodes

        max_geodes = geodes

        # Try building a geode robot
        if ore >= blueprint[5] and obsidian >= blueprint[6]:
            new_geodes = dfs(blueprint, time - 1, 
                             ore + ore_robots - blueprint[5], 
                             clay + clay_robots, 
                             obsidian + obsidian_robots - blueprint[6], 
                             geodes + geode_robots, 
                             ore_robots, clay_robots, obsidian_robots, geode_robots + 1)
            max_geodes = max(max_geodes, new_geodes)
            return max_geodes  # Prioritize geode robots

        # Try building an obsidian robot
        if ore >= blueprint[3] and clay >= blueprint[4]:
            new_geodes = dfs(blueprint, time - 1, 
                             ore + ore_robots - blueprint[3], 
                             clay + clay_robots - blueprint[4], 
                             obsidian + obsidian_robots, 
                             geodes + geode_robots, 
                             ore_robots, clay_robots, obsidian_robots + 1, geode_robots)
            max_geodes = max(max_geodes, new_geodes)

        # Try building a clay robot
        if ore >= blueprint[2]:
            new_geodes = dfs(blueprint, time - 1, 
                             ore + ore_robots - blueprint[2], 
                             clay + clay_robots, 
                             obsidian + obsidian_robots, 
                             geodes + geode_robots, 
                             ore_robots, clay_robots + 1, obsidian_robots, geode_robots)
            max_geodes = max(max_geodes, new_geodes)

        # Try building an ore robot
        if ore >= blueprint[1]:
            new_geodes = dfs(blueprint, time - 1, 
                             ore + ore_robots - blueprint[1], 
                             clay + clay_robots, 
                             obsidian + obsidian_robots, 
                             geodes + geode_robots, 
                             ore_robots + 1, clay_robots, obsidian_robots, geode_robots)
            max_geodes = max(max_geodes, new_geodes)

        # Don't build any robot
        new_geodes = dfs(blueprint, time - 1, 
                         ore + ore_robots, 
                         clay + clay_robots, 
                         obsidian + obsidian_robots, 
                         geodes + geode_robots, 
                         ore_robots, clay_robots, obsidian_robots, geode_robots)
        max_geodes = max(max_geodes, new_geodes)

        return max_geodes

    results = []
    for blueprint in blueprints:
        max_geodes = dfs(blueprint, time, 0, 0, 0, 0, 1, 0, 0, 0)
        results.append(max_geodes)
    return results

# Read input
with open('input.txt', 'r') as file:
    blueprints = [parse_blueprint(line.strip()) for line in file]

# Part 1
quality_levels = [i * geodes for i, geodes in enumerate(solve(blueprints, 24), 1)]
print(f"Part 1: {sum(quality_levels)}")

# Part 2
geodes_product = 1
for blueprint in blueprints[:3]:
    geodes_product *= solve([blueprint], 32)[0]
print(f"Part 2: {geodes_product}")
