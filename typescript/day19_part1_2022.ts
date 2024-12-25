
import * as fs from 'fs';

interface Blueprint {
  id: number;
  oreForOreRobot: number;
  oreForClayRobot: number;
  oreForObsidianRobot: number;
  clayForObsidianRobot: number;
  oreForGeodeRobot: number;
  obsidianForGeodeRobot: number;
}

interface State {
  blueprint: Blueprint;
  ore: number;
  clay: number;
  obsidian: number;
  geode: number;
  oreRobots: number;
  clayRobots: number;
  obsidianRobots: number;
  geodeRobots: number;
}

function newState(blueprint: Blueprint): State {
  return {
    blueprint,
    ore: 0,
    clay: 0,
    obsidian: 0,
    geode: 0,
    oreRobots: 1,
    clayRobots: 0,
    obsidianRobots: 0,
    geodeRobots: 0,
  };
}

function farm(s: State): void {
  s.ore += s.oreRobots;
  s.clay += s.clayRobots;
  s.obsidian += s.obsidianRobots;
  s.geode += s.geodeRobots;
}

function hash(s: State, time: number): string {
  return `${time},${s.ore},${s.clay},${s.obsidian},${s.geode},${s.oreRobots},${s.clayRobots},${s.obsidianRobots},${s.geodeRobots}`;
}

function copy(s: State): State {
  return { ...s };
}

function calcMostGeodes(
  s: State,
  time: number,
  memo: Map<string, number>,
  totalTime: number,
  earliestGeode: number
): number {
  if (time === totalTime) {
    return s.geode;
  }

  const h = hash(s, time);
  if (memo.has(h)) {
    return memo.get(h)!;
  }

  if (s.geode === 0 && time > earliestGeode) {
    return 0;
  }

  let mostGeodes = s.geode;
  const maxOreNeeded = Math.max(
    s.blueprint.oreForOreRobot,
    s.blueprint.oreForClayRobot,
    s.blueprint.oreForObsidianRobot,
    s.blueprint.oreForGeodeRobot
  );

  if (
    s.ore >= s.blueprint.oreForGeodeRobot &&
    s.obsidian >= s.blueprint.obsidianForGeodeRobot
  ) {
    const cp = copy(s);
    farm(cp);
    cp.ore -= cp.blueprint.oreForGeodeRobot;
    cp.obsidian -= cp.blueprint.obsidianForGeodeRobot;
    cp.geodeRobots++;
    if (cp.geodeRobots === 1) {
      earliestGeode = Math.min(earliestGeode, time + 1);
    }
    mostGeodes = Math.max(
      mostGeodes,
      calcMostGeodes(cp, time + 1, memo, totalTime, earliestGeode)
    );
    memo.set(h, mostGeodes);
    return mostGeodes;
  }

  if (
    time <= totalTime - 16 &&
    s.oreRobots < maxOreNeeded &&
    s.ore >= s.blueprint.oreForOreRobot
  ) {
    const cp = copy(s);
    cp.ore -= cp.blueprint.oreForOreRobot;
    farm(cp);
    cp.oreRobots++;
    mostGeodes = Math.max(
      mostGeodes,
      calcMostGeodes(cp, time + 1, memo, totalTime, earliestGeode)
    );
  }

  if (
    time <= totalTime - 8 &&
    s.clayRobots < s.blueprint.clayForObsidianRobot &&
    s.ore >= s.blueprint.oreForClayRobot
  ) {
    const cp = copy(s);
    cp.ore -= cp.blueprint.oreForClayRobot;
    farm(cp);
    cp.clayRobots++;
    mostGeodes = Math.max(
      mostGeodes,
      calcMostGeodes(cp, time + 1, memo, totalTime, earliestGeode)
    );
  }

  if (
    time <= totalTime - 4 &&
    s.obsidianRobots < s.blueprint.obsidianForGeodeRobot &&
    s.ore >= s.blueprint.oreForObsidianRobot &&
    s.clay >= s.blueprint.clayForObsidianRobot
  ) {
    const cp = copy(s);
    cp.ore -= cp.blueprint.oreForObsidianRobot;
    cp.clay -= cp.blueprint.clayForObsidianRobot;
    farm(cp);
    cp.obsidianRobots++;
    mostGeodes = Math.max(
      mostGeodes,
      calcMostGeodes(cp, time + 1, memo, totalTime, earliestGeode)
    );
  }

  const cp = copy(s);
  farm(cp);
  mostGeodes = Math.max(
    mostGeodes,
    calcMostGeodes(cp, time + 1, memo, totalTime, earliestGeode)
  );

  memo.set(h, mostGeodes);
  return mostGeodes;
}

function parseInput(input: string): Blueprint[] {
  const ans: Blueprint[] = [];
  const lines = input.split('\n');
  for (const line of lines) {
    const match = line.match(
      /Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian./
    );
    if (match) {
      const bp: Blueprint = {
        id: parseInt(match[1]),
        oreForOreRobot: parseInt(match[2]),
        oreForClayRobot: parseInt(match[3]),
        oreForObsidianRobot: parseInt(match[4]),
        clayForObsidianRobot: parseInt(match[5]),
        oreForGeodeRobot: parseInt(match[6]),
        obsidianForGeodeRobot: parseInt(match[7]),
      };
      ans.push(bp);
    }
  }
  return ans;
}

function part1(input: string): number {
  const blueprints = parseInput(input);
  let sum = 0;
  for (const bp of blueprints) {
    const st = newState(bp);
    const geodesMade = calcMostGeodes(st, 0, new Map<string, number>(), 24, 24);
    sum += st.blueprint.id * geodesMade;
  }
  return sum;
}

const input = fs.readFileSync('input.txt', 'utf-8').trim();
console.log(part1(input));
