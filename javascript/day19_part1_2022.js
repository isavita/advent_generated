const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

const parseInput = (input) => {
  return input.split('\n').map((line) => {
    const [, id, oreForOreRobot, oreForClayRobot, oreForObsidianRobot, clayForObsidianRobot, oreForGeodeRobot, obsidianForGeodeRobot] = line.match(/Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian./);
    return {
      id: parseInt(id),
      oreForOreRobot: parseInt(oreForOreRobot),
      oreForClayRobot: parseInt(oreForClayRobot),
      oreForObsidianRobot: parseInt(oreForObsidianRobot),
      clayForObsidianRobot: parseInt(clayForObsidianRobot),
      oreForGeodeRobot: parseInt(oreForGeodeRobot),
      obsidianForGeodeRobot: parseInt(obsidianForGeodeRobot),
    };
  });
};

const newState = (blueprint) => ({
  ...blueprint,
  ore: 0,
  clay: 0,
  obsidian: 0,
  geode: 0,
  oreRobots: 1,
  clayRobots: 0,
  obsidianRobots: 0,
  geodeRobots: 0,
});

const calcMostGeodes = (state, time, memo, totalTime, earliestGeode) => {
  if (time === totalTime) {
    return state.geode;
  }

  const hash = `${time},${state.ore},${state.clay},${state.obsidian},${state.geode},${state.oreRobots},${state.clayRobots},${state.obsidianRobots},${state.geodeRobots}`;
  if (memo.has(hash)) {
    return memo.get(hash);
  }

  if (state.geode === 0 && time > earliestGeode) {
    return 0;
  }

  let mostGeodes = state.geode;

  if (state.ore >= state.oreForGeodeRobot && state.obsidian >= state.obsidianForGeodeRobot) {
    const newState = { ...state };
    newState.ore += newState.oreRobots;
    newState.clay += newState.clayRobots;
    newState.obsidian += newState.obsidianRobots;
    newState.geode += newState.geodeRobots;
    newState.ore -= newState.oreForGeodeRobot;
    newState.obsidian -= newState.obsidianForGeodeRobot;
    newState.geodeRobots++;
    if (newState.geodeRobots === 1) {
      earliestGeode = Math.min(earliestGeode, time + 1);
    }
    mostGeodes = Math.max(mostGeodes, calcMostGeodes(newState, time + 1, memo, totalTime, earliestGeode));
    memo.set(hash, mostGeodes);
    return mostGeodes;
  }

  if (time <= totalTime - 16 && state.oreRobots < state.oreForObsidianRobot * 2 && state.ore >= state.oreForOreRobot) {
    const newState = { ...state };
    newState.ore -= newState.oreForOreRobot;
    newState.ore += newState.oreRobots;
    newState.clay += newState.clayRobots;
    newState.obsidian += newState.obsidianRobots;
    newState.geode += newState.geodeRobots;
    newState.oreRobots++;
    mostGeodes = Math.max(mostGeodes, calcMostGeodes(newState, time + 1, memo, totalTime, earliestGeode));
  }

  if (time <= totalTime - 8 && state.clayRobots < state.clayForObsidianRobot && state.ore >= state.oreForClayRobot) {
    const newState = { ...state };
    newState.ore -= newState.oreForClayRobot;
    newState.ore += newState.oreRobots;
    newState.clay += newState.clayRobots;
    newState.obsidian += newState.obsidianRobots;
    newState.geode += newState.geodeRobots;
    newState.clayRobots++;
    mostGeodes = Math.max(mostGeodes, calcMostGeodes(newState, time + 1, memo, totalTime, earliestGeode));
  }

  if (time <= totalTime - 4 && state.obsidianRobots < state.obsidianForGeodeRobot && state.ore >= state.oreForObsidianRobot && state.clay >= state.clayForObsidianRobot) {
    const newState = { ...state };
    newState.ore -= newState.oreForObsidianRobot;
    newState.clay -= newState.clayForObsidianRobot;
    newState.ore += newState.oreRobots;
    newState.clay += newState.clayRobots;
    newState.obsidian += newState.obsidianRobots;
    newState.geode += newState.geodeRobots;
    newState.obsidianRobots++;
    mostGeodes = Math.max(mostGeodes, calcMostGeodes(newState, time + 1, memo, totalTime, earliestGeode));
  }

  const newState = { ...state };
  newState.ore += newState.oreRobots;
  newState.clay += newState.clayRobots;
  newState.obsidian += newState.obsidianRobots;
  newState.geode += newState.geodeRobots;
  mostGeodes = Math.max(mostGeodes, calcMostGeodes(newState, time + 1, memo, totalTime, earliestGeode));

  memo.set(hash, mostGeodes);
  return mostGeodes;
};

const part1 = (input) => {
  const blueprints = parseInput(input);

  let sum = 0;
  for (const blueprint of blueprints) {
    const state = newState(blueprint);
    const geodesMade = calcMostGeodes(state, 0, new Map(), 24, 24);
    sum += state.id * geodesMade;
  }

  return sum;
};

console.log(part1(input));