from collections import namedtuple
import re

Blueprint = namedtuple('Blueprint', ['id', 'ore_robot', 'clay_robot', 'obsidian_robot', 'geode_robot'])

def parse_blueprint(line):
    numbers = list(map(int, re.findall(r'\d+', line)))
    return Blueprint(numbers[0], numbers[1], numbers[2], (numbers[3], numbers[4]), (numbers[5], numbers[6]))

def max_geodes(blueprint, time=24):
    ore_robot_cost, clay_robot_cost, obsidian_robot_cost, geode_robot_cost = blueprint[1:]
    max_ore_cost = max(ore_robot_cost, clay_robot_cost, obsidian_robot_cost[0], geode_robot_cost[0])

    def dfs(time, ore, clay, obsidian, geodes, ore_robots, clay_robots, obsidian_robots, geode_robots):
        if time == 0:
            return geodes

        key = (time, ore, clay, obsidian, geodes, ore_robots, clay_robots, obsidian_robots, geode_robots)
        if key in memo:
            return memo[key]

        max_geodes = geodes + geode_robots * time

        # Prune if it's impossible to beat the current best
        if max_geodes + (time * (time - 1) // 2) <= best[0]:
            return max_geodes

        # Try building each type of robot
        if ore >= geode_robot_cost[0] and obsidian >= geode_robot_cost[1]:
            score = dfs(time - 1, ore + ore_robots - geode_robot_cost[0], clay + clay_robots,
                        obsidian + obsidian_robots - geode_robot_cost[1], geodes + geode_robots,
                        ore_robots, clay_robots, obsidian_robots, geode_robots + 1)
            max_geodes = max(max_geodes, score)
        else:
            if ore >= obsidian_robot_cost[0] and clay >= obsidian_robot_cost[1] and obsidian_robots < geode_robot_cost[1]:
                score = dfs(time - 1, ore + ore_robots - obsidian_robot_cost[0], clay + clay_robots - obsidian_robot_cost[1],
                            obsidian + obsidian_robots, geodes + geode_robots,
                            ore_robots, clay_robots, obsidian_robots + 1, geode_robots)
                max_geodes = max(max_geodes, score)

            if ore >= clay_robot_cost and clay_robots < obsidian_robot_cost[1]:
                score = dfs(time - 1, ore + ore_robots - clay_robot_cost, clay + clay_robots,
                            obsidian + obsidian_robots, geodes + geode_robots,
                            ore_robots, clay_robots + 1, obsidian_robots, geode_robots)
                max_geodes = max(max_geodes, score)

            if ore >= ore_robot_cost and ore_robots < max_ore_cost:
                score = dfs(time - 1, ore + ore_robots - ore_robot_cost, clay + clay_robots,
                            obsidian + obsidian_robots, geodes + geode_robots,
                            ore_robots + 1, clay_robots, obsidian_robots, geode_robots)
                max_geodes = max(max_geodes, score)

            # Do nothing and just collect resources
            score = dfs(time - 1, ore + ore_robots, clay + clay_robots,
                        obsidian + obsidian_robots, geodes + geode_robots,
                        ore_robots, clay_robots, obsidian_robots, geode_robots)
            max_geodes = max(max_geodes, score)

        memo[key] = max_geodes
        best[0] = max(best[0], max_geodes)
        return max_geodes

    memo = {}
    best = [0]
    return dfs(time, 0, 0, 0, 0, 1, 0, 0, 0)

def solve(blueprints):
    total_quality = 0
    for blueprint in blueprints:
        geodes = max_geodes(blueprint)
        total_quality += blueprint.id * geodes
    return total_quality

# Example usage
blueprints = [
    parse_blueprint("Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."),
    parse_blueprint("Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")
]

print(solve(blueprints))
