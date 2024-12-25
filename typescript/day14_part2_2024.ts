
type Robot = {
  x: number;
  y: number;
  vx: number;
  vy: number;
};

const mod = (a: number, b: number): number => ((a % b) + b) % b;

const parseLine = (line: string): Robot => {
  const matches = line.match(/p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/);
  if (!matches || matches.length !== 5) {
    throw new Error(`Invalid line format: ${line}`);
  }
  return {
    x: parseInt(matches[1]),
    y: parseInt(matches[2]),
    vx: parseInt(matches[3]),
    vy: parseInt(matches[4]),
  };
};

const moveRobots = (robots: Robot[], sizeX: number, sizeY: number): void => {
  for (const robot of robots) {
    robot.x = mod(robot.x + robot.vx, sizeX);
    robot.y = mod(robot.y + robot.vy, sizeY);
  }
};

const countQuadrants = (
  robots: Robot[],
  sizeX: number,
  sizeY: number
): number[] => {
  const counts = [0, 0, 0, 0];
  const centerX = sizeX / 2;
  const centerY = sizeY / 2;
  for (const robot of robots) {
    const { x, y } = robot;
    if (x < centerX) {
      if (y < centerY) {
        counts[0]++;
      } else if (y > centerY) {
        counts[1]++;
      }
    } else if (x > centerX) {
      if (y < centerY) {
        counts[2]++;
      } else if (y > centerY) {
        counts[3]++;
      }
    }
  }
  return counts;
};

const hasNoOverlaps = (robots: Robot[]): boolean => {
  const positionMap = new Map<string, boolean>();
  for (const robot of robots) {
    const pos = `${robot.x},${robot.y}`;
    if (positionMap.has(pos)) {
      return false;
    }
    positionMap.set(pos, true);
  }
  return true;
};

const drawGrid = (robots: Robot[], sizeX: number, sizeY: number): void => {
  const gridMap = new Map<string, boolean>();
  for (const robot of robots) {
    gridMap.set(`${robot.x},${robot.y}`, true);
  }
  for (let y = 0; y < sizeY; y++) {
    let line = "";
    for (let x = 0; x < sizeX; x++) {
      line += gridMap.has(`${x},${y}`) ? "#" : ".";
    }
    console.log(line);
  }
};

const fs = require("fs");
const sizeX = 101;
const sizeY = 103;

const fileContent = fs.readFileSync("input.txt", "utf-8");
const lines = fileContent.split("\n");
const robots: Robot[] = [];
for (const line of lines) {
  if (line.trim() === "") continue;
  robots.push(parseLine(line));
}

const robotsPart1 = structuredClone(robots);
for (let n = 0; n < 100; n++) {
  moveRobots(robotsPart1, sizeX, sizeY);
}
const counts = countQuadrants(robotsPart1, sizeX, sizeY);
let safetyFactor = 1;
for (const c of counts) {
  safetyFactor *= c;
}
console.log(`Part 1 - Safety Factor after 100 seconds: ${safetyFactor}`);

const robotsPart2 = structuredClone(robots);
let seconds = 0;
while (true) {
  if (hasNoOverlaps(robotsPart2)) {
    break;
  }
  moveRobots(robotsPart2, sizeX, sizeY);
  seconds++;
  if (seconds > 1000000) {
    console.log(
      "Exceeded maximum iterations without finding a unique position configuration."
    );
    process.exit(1);
  }
}
console.log(`Part 2 - Fewest seconds to display Easter egg: ${seconds}`);
console.log("Final positions of robots:");
drawGrid(robotsPart2, sizeX, sizeY);
