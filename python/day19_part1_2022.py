import re
from collections import namedtuple
from functools import lru_cache

Blueprint = namedtuple('Blueprint', ['id', 'ore_robot', 'clay_robot', 'obsidian_robot', 'geode_robot'])

def parse_blueprint(line):
    numbers = list(map(int, re.findall(r'\d+', line)))
    return Blueprint(
        id=numbers[0],
        ore_robot=numbers[1],
        clay_robot=numbers[2],
        obsidian_robot=(numbers[3], numbers[4]),
        geode_robot=(numbers[5], numbers[6])
    )

def simulate(blueprint, time):
    max_ore_cost = max(blueprint.ore_robot, blueprint.clay_robot, blueprint.obsidian_robot[0], blueprint.geode_robot[0])

    @lru_cache(maxsize=None)
    def dfs(time, ore, clay, obsidian, ore_robots, clay_robots, obsidian_robots, geode):
        if time == 0:
            return geode

        # Upper bound estimation
        if geode + (time * (time - 1)) // 2 <= max_geodes[0]:
            return 0

        max_geode = geode

        # Build geode robot
        if ore >= blueprint.geode_robot[0] and obsidian >= blueprint.geode_robot[1]:
            return dfs(time - 1,
                       ore + ore_robots - blueprint.geode_robot[0],
                       clay + clay_robots,
                       obsidian + obsidian_robots - blueprint.geode_robot[1],
                       ore_robots, clay_robots, obsidian_robots,
                       geode + time - 1)

        # Build obsidian robot
        if ore >= blueprint.obsidian_robot[0] and clay >= blueprint.obsidian_robot[1] and obsidian_robots < blueprint.geode_robot[1]:
            max_geode = max(max_geode, dfs(time - 1,
                                           ore + ore_robots - blueprint.obsidian_robot[0],
                                           clay + clay_robots - blueprint.obsidian_robot[1],
                                           obsidian + obsidian_robots,
                                           ore_robots, clay_robots, obsidian_robots + 1,
                                           geode))

        # Build clay robot
        if ore >= blueprint.clay_robot and clay_robots < blueprint.obsidian_robot[1]:
            max_geode = max(max_geode, dfs(time - 1,
                                           ore + ore_robots - blueprint.clay_robot,
                                           clay + clay_robots,
                                           obsidian + obsidian_robots,
                                           ore_robots, clay_robots + 1, obsidian_robots,
                                           geode))

        # Build ore robot
        if ore >= blueprint.ore_robot and ore_robots < max_ore_cost:
            max_geode = max(max_geode, dfs(time - 1,
                                           ore + ore_robots - blueprint.ore_robot,
                                           clay + clay_robots,
                                           obsidian + obsidian_robots,
                                           ore_robots + 1, clay_robots, obsidian_robots,
                                           geode))

        # Don't build any robot
        max_geode = max(max_geode, dfs(time - 1,
                                       min(ore + ore_robots, time * max_ore_cost - ore_robots * (time - 1)),
                                       clay + clay_robots,
                                       obsidian + obsidian_robots,
                                       ore_robots, clay_robots, obsidian_robots,
                                       geode))

        max_geodes[0] = max(max_geodes[0], max_geode)
        return max_geode

    max_geodes = [0]
    return dfs(time, 0, 0, 0, 1, 0, 0, 0)

def solve(blueprints, time=24):
    total_quality = 0
    for blueprint in blueprints:
        geodes = simulate(blueprint, time)
        quality = blueprint.id * geodes
        total_quality += quality
    return total_quality

def main():
    with open('input.txt', 'r') as file:
        blueprints = [parse_blueprint(line.strip()) for line in file]
    
    result = solve(blueprints)
    print(f"The sum of the quality levels of all blueprints is: {result}")

if __name__ == "__main__":
    main()
