
import 'dart:io';
import 'dart:math';

class Blueprint {
  int id;
  int oreRobotOreCost;
  int clayRobotOreCost;
  int obsidianRobotOreCost;
  int obsidianRobotClayCost;
  int geodeRobotOreCost;
  int geodeRobotObsidianCost;

  Blueprint(
      this.id,
      this.oreRobotOreCost,
      this.clayRobotOreCost,
      this.obsidianRobotOreCost,
      this.obsidianRobotClayCost,
      this.geodeRobotOreCost,
      this.geodeRobotObsidianCost);
}

class State {
  int ore;
  int clay;
  int obsidian;
  int geodes;
  int oreRobots;
  int clayRobots;
  int obsidianRobots;
  int geodeRobots;
  int minutesLeft;

  State(
      this.ore,
      this.clay,
      this.obsidian,
      this.geodes,
      this.oreRobots,
      this.clayRobots,
      this.obsidianRobots,
      this.geodeRobots,
      this.minutesLeft);

  State copy() {
    return State(ore, clay, obsidian, geodes, oreRobots, clayRobots,
        obsidianRobots, geodeRobots, minutesLeft);
  }
}

int maxGeodes(Blueprint blueprint, int timeLimit) {
  int maxOreCost = max(
      max(blueprint.oreRobotOreCost, blueprint.clayRobotOreCost),
      max(blueprint.obsidianRobotOreCost, blueprint.geodeRobotOreCost));

  int best = 0;
  Set<String> visited = {};
  List<State> queue = [State(0, 0, 0, 0, 1, 0, 0, 0, timeLimit)];

  while (queue.isNotEmpty) {
    State current = queue.removeLast();

    if (current.minutesLeft == 0) {
      best = max(best, current.geodes);
      continue;
    }

    String hash =
        '${current.ore},${current.clay},${current.obsidian},${current.geodes},'
        '${current.oreRobots},${current.clayRobots},${current.obsidianRobots},'
        '${current.geodeRobots},${current.minutesLeft}';
    if (visited.contains(hash)) continue;
    visited.add(hash);
    
    
    int potentialGeodes = current.geodes + current.geodeRobots * current.minutesLeft + current.minutesLeft * (current.minutesLeft-1) ~/ 2;
    if (potentialGeodes <= best) continue;
    

    // Option: Build nothing
    State next = current.copy();
    next.ore += next.oreRobots;
    next.clay += next.clayRobots;
    next.obsidian += next.obsidianRobots;
    next.geodes += next.geodeRobots;
    next.minutesLeft--;
    queue.add(next);

    // Option: Build ore robot
    if (current.ore >= blueprint.oreRobotOreCost &&
        current.oreRobots < maxOreCost) {
      next = current.copy();
      next.ore -= blueprint.oreRobotOreCost;
      next.ore += next.oreRobots;
      next.clay += next.clayRobots;
      next.obsidian += next.obsidianRobots;
      next.geodes += next.geodeRobots;
      next.minutesLeft--;
      next.oreRobots++;
      queue.add(next);
    }

    // Option: Build clay robot
    if (current.ore >= blueprint.clayRobotOreCost &&
        current.clayRobots < blueprint.obsidianRobotClayCost) {
      next = current.copy();
      next.ore -= blueprint.clayRobotOreCost;
      next.ore += next.oreRobots;
      next.clay += next.clayRobots;
      next.obsidian += next.obsidianRobots;
      next.geodes += next.geodeRobots;
      next.minutesLeft--;
      next.clayRobots++;
      queue.add(next);
    }

    // Option: Build obsidian robot
    if (current.ore >= blueprint.obsidianRobotOreCost &&
        current.clay >= blueprint.obsidianRobotClayCost &&
        current.obsidianRobots < blueprint.geodeRobotObsidianCost) {
      next = current.copy();
      next.ore -= blueprint.obsidianRobotOreCost;
      next.clay -= blueprint.obsidianRobotClayCost;
      next.ore += next.oreRobots;
      next.clay += next.clayRobots;
      next.obsidian += next.obsidianRobots;
      next.geodes += next.geodeRobots;
      next.minutesLeft--;
      next.obsidianRobots++;
      queue.add(next);
    }

    // Option: Build geode robot
    if (current.ore >= blueprint.geodeRobotOreCost &&
        current.obsidian >= blueprint.geodeRobotObsidianCost) {
      next = current.copy();
      next.ore -= blueprint.geodeRobotOreCost;
      next.obsidian -= blueprint.geodeRobotObsidianCost;
      next.ore += next.oreRobots;
      next.clay += next.clayRobots;
      next.obsidian += next.obsidianRobots;
      next.geodes += next.geodeRobots;
      next.minutesLeft--;
      next.geodeRobots++;
      queue.add(next);
    }
  }

  return best;
}

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  List<Blueprint> blueprints = [];

  for (String line in lines) {
    List<String> parts = line.split(' ');
    int id = int.parse(parts[1].substring(0, parts[1].length - 1));
    int oreRobotOreCost = int.parse(parts[6]);
    int clayRobotOreCost = int.parse(parts[12]);
    int obsidianRobotOreCost = int.parse(parts[18]);
    int obsidianRobotClayCost = int.parse(parts[21]);
    int geodeRobotOreCost = int.parse(parts[27]);
    int geodeRobotObsidianCost = int.parse(parts[30]);
    blueprints.add(Blueprint(id, oreRobotOreCost, clayRobotOreCost,
        obsidianRobotOreCost, obsidianRobotClayCost, geodeRobotOreCost,
        geodeRobotObsidianCost));
  }

  // Part 1
  int totalQualityLevel = 0;
  for (Blueprint blueprint in blueprints) {
    int max = maxGeodes(blueprint, 24);
    totalQualityLevel += blueprint.id * max;
  }
  print('Part 1: $totalQualityLevel');

  // Part 2
  int product = 1;
  for (int i = 0; i < min(3, blueprints.length); i++) {
    int max = maxGeodes(blueprints[i], 32);
    product *= max;
  }
  print('Part 2: $product');
}
