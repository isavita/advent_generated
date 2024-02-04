const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const nanobots = input.map(line => {
  const [, x, y, z, radius] = line.match(/pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/).map(Number);
  return { x, y, z, radius };
});

const findStrongestNanobot = (nanobots) => {
  return nanobots.reduce((strongest, nanobot) => nanobot.radius > strongest.radius ? nanobot : strongest, nanobots[0]);
};

const countNanobotsInRange = (nanobots, strongest) => {
  return nanobots.reduce((count, nanobot) => manhattanDistance(nanobot, strongest) <= strongest.radius ? count + 1 : count, 0);
};

const manhattanDistance = (a, b) => {
  return Math.abs(a.x - b.x) + Math.abs(a.y - b.y) + Math.abs(a.z - b.z);
};

const strongest = findStrongestNanobot(nanobots);
const inRangeCount = countNanobotsInRange(nanobots, strongest);

console.log(inRangeCount);