import re
from functools import lru_cache

def parse_blueprint(line):
    numbers = list(map(int, re.findall(r'\d+', line)))
    return {
        'id': numbers[0],
        'ore': {'ore': numbers[1]},
        'clay': {'ore': numbers[2]},
        'obsidian': {'ore': numbers[3], 'clay': numbers[4]},
        'geode': {'ore': numbers[5], 'obsidian': numbers[6]}
    }

def simulate_blueprint(blueprint, time=24):
    max_ore_cost = max(blueprint[r]['ore'] for r in ['ore', 'clay', 'obsidian', 'geode'])
    max_clay_cost = blueprint['obsidian']['clay']
    max_obsidian_cost = blueprint['geode']['obsidian']

    @lru_cache(maxsize=None)
    def dfs(time, ore, clay, obsidian, geodes, ore_robots, clay_robots, obsidian_robots, geode_robots):
        if time == 0:
            return geodes

        max_geodes = geodes

        # Try building each type of robot
        for robot_type in ['geode', 'obsidian', 'clay', 'ore']:
            if robot_type == 'ore' and ore_robots >= max_ore_cost:
                continue
            if robot_type == 'clay' and clay_robots >= max_clay_cost:
                continue
            if robot_type == 'obsidian' and obsidian_robots >= max_obsidian_cost:
                continue

            cost = blueprint[robot_type]
            if all(resources >= cost.get(resource, 0) for resource, resources in [('ore', ore), ('clay', clay), ('obsidian', obsidian)]):
                new_ore = ore + ore_robots - cost.get('ore', 0)
                new_clay = clay + clay_robots - cost.get('clay', 0)
                new_obsidian = obsidian + obsidian_robots - cost.get('obsidian', 0)
                new_geodes = geodes + geode_robots

                if robot_type == 'ore':
                    result = dfs(time-1, new_ore, new_clay, new_obsidian, new_geodes, ore_robots+1, clay_robots, obsidian_robots, geode_robots)
                elif robot_type == 'clay':
                    result = dfs(time-1, new_ore, new_clay, new_obsidian, new_geodes, ore_robots, clay_robots+1, obsidian_robots, geode_robots)
                elif robot_type == 'obsidian':
                    result = dfs(time-1, new_ore, new_clay, new_obsidian, new_geodes, ore_robots, clay_robots, obsidian_robots+1, geode_robots)
                else:  # geode
                    result = dfs(time-1, new_ore, new_clay, new_obsidian, new_geodes, ore_robots, clay_robots, obsidian_robots, geode_robots+1)

                max_geodes = max(max_geodes, result)

        # Try not building any robot
        max_geodes = max(max_geodes, dfs(time-1, ore+ore_robots, clay+clay_robots, obsidian+obsidian_robots, geodes+geode_robots,
                                         ore_robots, clay_robots, obsidian_robots, geode_robots))

        return max_geodes

    return dfs(time, 0, 0, 0, 0, 1, 0, 0, 0)

def solve(blueprints):
    total_quality = 0
    for blueprint in blueprints:
        max_geodes = simulate_blueprint(blueprint)
        total_quality += blueprint['id'] * max_geodes
    return total_quality

# Read input from file
with open('input.txt', 'r') as file:
    blueprints = [parse_blueprint(line.strip()) for line in file]

# Solve and print the result
result = solve(blueprints)
print(result)
