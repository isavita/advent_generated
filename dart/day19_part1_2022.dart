import 'dart:io';
import 'dart:math';

class Blueprint {
  int id;
  int oreForOreRobot;
  int oreForClayRobot;
  int oreForObsidianRobot, clayForObsidianRobot;
  int oreForGeodeRobot, obsidianForGeodeRobot;

  Blueprint(this.id, this.oreForOreRobot, this.oreForClayRobot, this.oreForObsidianRobot,
      this.clayForObsidianRobot, this.oreForGeodeRobot, this.obsidianForGeodeRobot);
}

class State {
  Blueprint blueprint;
  int ore, clay, obsidian, geode;
  int oreRobots, clayRobots, obsidianRobots, geodeRobots;

  State(this.blueprint)
      : ore = 0,
        clay = 0,
        obsidian = 0,
        geode = 0,
        oreRobots = 1,
        clayRobots = 0,
        obsidianRobots = 0,
        geodeRobots = 0;

  void farm() {
    ore += oreRobots;
    clay += clayRobots;
    obsidian += obsidianRobots;
    geode += geodeRobots;
  }

  String hash(int time) {
    return '$time,$ore,$clay,$obsidian,$geode,$oreRobots,$clayRobots,$obsidianRobots,$geodeRobots';
  }

  State copy() {
    return State(blueprint)
      ..ore = ore
      ..clay = clay
      ..obsidian = obsidian
      ..geode = geode
      ..oreRobots = oreRobots
      ..clayRobots = clayRobots
      ..obsidianRobots = obsidianRobots
      ..geodeRobots = geodeRobots;
  }

  int calcMostGeodes(int time, Map<String, int> memo, int totalTime, int earliestGeode) {
    if (time == totalTime) return geode;

    final h = hash(time);
    if (memo.containsKey(h)) return memo[h]!;

    if (geode == 0 && time > earliestGeode) return 0;

    var mostGeodes = geode;

    if (ore >= blueprint.oreForGeodeRobot && obsidian >= blueprint.obsidianForGeodeRobot) {
      final cp = copy();
      cp.farm();
      cp.ore -= blueprint.oreForGeodeRobot;
      cp.obsidian -= blueprint.obsidianForGeodeRobot;
      cp.geodeRobots++;
      if (cp.geodeRobots == 1) earliestGeode = min(earliestGeode, time + 1);
      mostGeodes = max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));
      memo[h] = mostGeodes;
      return mostGeodes;
    }

    if (time <= totalTime - 16 && oreRobots < blueprint.oreForObsidianRobot * 2 && ore >= blueprint.oreForOreRobot) {
      final cp = copy();
      cp.ore -= blueprint.oreForOreRobot;
      cp.farm();
      cp.oreRobots++;
      mostGeodes = max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));
    }
    if (time <= totalTime - 8 && clayRobots < blueprint.clayForObsidianRobot && ore >= blueprint.oreForClayRobot) {
      final cp = copy();
      cp.ore -= blueprint.oreForClayRobot;
      cp.farm();
      cp.clayRobots++;
      mostGeodes = max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));
    }
    if (time <= totalTime - 4 && obsidianRobots < blueprint.obsidianForGeodeRobot && ore >= blueprint.oreForObsidianRobot && clay >= blueprint.clayForObsidianRobot) {
      final cp = copy();
      cp.ore -= blueprint.oreForObsidianRobot;
      cp.clay -= blueprint.clayForObsidianRobot;
      cp.farm();
      cp.obsidianRobots++;
      mostGeodes = max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));
    }

    final cp = copy();
    cp.farm();
    mostGeodes = max(mostGeodes, cp.calcMostGeodes(time + 1, memo, totalTime, earliestGeode));

    memo[h] = mostGeodes;
    return mostGeodes;
  }
}

void main() {
  final input = File('input.txt').readAsStringSync().trim().split('\n');
  final blueprints = <Blueprint>[];

  for (final line in input) {
    final parts = line.split(': ');
    final id = int.parse(parts[0].split(' ')[1]);
    final costs = parts[1].split('. ');
    final oreForOreRobot = int.parse(costs[0].split(' ')[4]);
    final oreForClayRobot = int.parse(costs[1].split(' ')[4]);
    final oreForObsidianRobot = int.parse(costs[2].split(' ')[4]);
    final clayForObsidianRobot = int.parse(costs[2].split(' ')[7]);
    final oreForGeodeRobot = int.parse(costs[3].split(' ')[4]);
    final obsidianForGeodeRobot = int.parse(costs[3].split(' ')[7]);
    blueprints.add(Blueprint(id, oreForOreRobot, oreForClayRobot, oreForObsidianRobot, clayForObsidianRobot, oreForGeodeRobot, obsidianForGeodeRobot));
  }

  int sum = 0;
  for (final bp in blueprints) {
    final st = State(bp);
    final geodesMade = st.calcMostGeodes(0, {}, 24, 24);
    sum += bp.id * geodesMade;
  }

  print(sum);
}