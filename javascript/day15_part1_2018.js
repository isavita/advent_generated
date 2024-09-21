const fs = require('fs');
const path = require('path');

// Constants for unit types
const ELF = 'E';
const GOBLIN = 'G';

// Directions for movement (Up, Left, Right, Down)
const DIRECTIONS = [
  { dx: 0, dy: -1 }, // Up
  { dx: -1, dy: 0 }, // Left
  { dx: 1, dy: 0 },  // Right
  { dx: 0, dy: 1 },  // Down
];

// Unit class representing each Elf or Goblin
class Unit {
  constructor(type, x, y) {
    this.type = type; // 'E' for Elf, 'G' for Goblin
    this.x = x;
    this.y = y;
    this.hp = 200;
    this.attackPower = 3;
    this.alive = true;
  }

  position() {
    return `${this.x},${this.y}`;
  }
}

// Read and parse the input map
function parseInput(filePath) {
  const input = fs.readFileSync(path.resolve(__dirname, filePath), 'utf-8');
  const grid = [];
  const units = [];

  input.split('\n').forEach((line, y) => {
    const row = [];
    for (let x = 0; x < line.length; x++) {
      const char = line[x];
      if (char === ELF || char === GOBLIN) {
        units.push(new Unit(char, x, y)); // Corrected parameter order
        row.push('.');
      } else {
        row.push(char);
      }
    }
    grid.push(row);
  });

  return { grid, units };
}

// Sort units in reading order
function sortUnits(units) {
  units.sort((a, b) => {
    if (a.y !== b.y) return a.y - b.y;
    return a.x - b.x;
  });
}

// Get adjacent positions in reading order
function getAdjacent(x, y) {
  return [
    { x, y: y - 1 },     // Up
    { x: x - 1, y },     // Left
    { x: x + 1, y },     // Right
    { x, y: y + 1 },     // Down
  ];
}

// Check if two positions are equal
function isEqual(pos1, pos2) {
  return pos1.x === pos2.x && pos1.y === pos2.y;
}

// Find targets for a unit
function findTargets(unit, units) {
  return units.filter(u => u.alive && u.type !== unit.type);
}

// Get open squares adjacent to targets
function getInRangeSquares(targets, grid, units) {
  const inRange = new Set();
  targets.forEach(target => {
    getAdjacent(target.x, target.y).forEach(pos => {
      if (
        grid[pos.y] &&
        grid[pos.y][pos.x] === '.' &&
        !units.some(u => u.alive && u.x === pos.x && u.y === pos.y)
      ) {
        inRange.add(`${pos.x},${pos.y}`);
      }
    });
  });
  return Array.from(inRange).map(pos => {
    const [x, y] = pos.split(',').map(Number);
    return { x, y };
  });
}

// Breadth-First Search to find the shortest path
function bfs(start, targets, grid, units) {
  const queue = [];
  const visited = Array.from({ length: grid.length }, () =>
    Array(grid[0].length).fill(false)
  );
  const distances = Array.from({ length: grid.length }, () =>
    Array(grid[0].length).fill(Infinity)
  );
  const paths = Array.from({ length: grid.length }, () =>
    Array(grid[0].length).fill(null)
  );

  queue.push(start);
  visited[start.y][start.x] = true;
  distances[start.y][start.x] = 0;

  let foundDistance = Infinity;
  const reachable = [];

  while (queue.length > 0) {
    const current = queue.shift();
    const currentDistance = distances[current.y][current.x];

    if (currentDistance > foundDistance) break;

    // Check if current is a target
    if (targets.some(t => isEqual(t, current))) {
      foundDistance = currentDistance;
      reachable.push(current);
      continue;
    }

    // Explore neighbors in reading order
    getAdjacent(current.x, current.y).forEach(next => {
      if (
        grid[next.y] &&
        grid[next.y][next.x] === '.' &&
        !units.some(u => u.alive && u.x === next.x && u.y === next.y) &&
        !visited[next.y][next.x]
      ) {
        visited[next.y][next.x] = true;
        distances[next.y][next.x] = currentDistance + 1;
        paths[next.y][next.x] = current;
        queue.push(next);
      }
    });
  }

  if (reachable.length === 0) return null;

  // Sort reachable targets in reading order
  reachable.sort((a, b) => {
    if (a.y !== b.y) return a.y - b.y;
    return a.x - b.x;
  });

  const chosen = reachable[0];

  // Backtrack to find the first step
  let step = chosen;
  while (!isEqual(paths[step.y][step.x], start)) {
    step = paths[step.y][step.x];
  }

  return step;
}

// Execute one round of combat
function executeRound(grid, units) {
  sortUnits(units);
  for (let unit of units) {
    if (!unit.alive) continue;

    const targets = findTargets(unit, units);
    if (targets.length === 0) return false; // Combat ends

    // Check if in range of any target
    const adjacentEnemies = getAdjacent(unit.x, unit.y).filter(pos =>
      units.some(
        u =>
          u.alive &&
          u.type !== unit.type &&
          u.x === pos.x &&
          u.y === pos.y
      )
    );

    if (adjacentEnemies.length === 0) {
      // Need to move
      const inRangeSquares = getInRangeSquares(targets, grid, units);
      if (inRangeSquares.length === 0) continue; // No reachable targets

      const step = bfs({ x: unit.x, y: unit.y }, inRangeSquares, grid, units);
      if (step) {
        unit.x = step.x;
        unit.y = step.y;

        // After moving, check for enemies in range
        const newAdjacentEnemies = getAdjacent(unit.x, unit.y).filter(pos =>
          units.some(
            u =>
              u.alive &&
              u.type !== unit.type &&
              u.x === pos.x &&
              u.y === pos.y
          )
        );

        if (newAdjacentEnemies.length > 0) {
          // Attack
          attack(unit, units);
        }
      }
    } else {
      // Attack
      attack(unit, units);
    }
  }
  return true; // Round completed
}

// Attack logic
function attack(unit, units) {
  const enemiesInRange = getAdjacent(unit.x, unit.y)
    .map(pos =>
      units.find(
        u =>
          u.alive &&
          u.type !== unit.type &&
          u.x === pos.x &&
          u.y === pos.y
      )
    )
    .filter(u => u !== undefined);

  if (enemiesInRange.length === 0) return;

  // Select enemy with lowest HP, then reading order
  enemiesInRange.sort((a, b) => {
    if (a.hp !== b.hp) return a.hp - b.hp;
    if (a.y !== b.y) return a.y - b.y;
    return a.x - b.x;
  });

  const target = enemiesInRange[0];
  target.hp -= unit.attackPower;
  if (target.hp <= 0) {
    target.alive = false;
  }
}

// Calculate the outcome
function calculateOutcome(rounds, units) {
  const totalHP = units
    .filter(u => u.alive)
    .reduce((sum, u) => sum + u.hp, 0);
  return rounds * totalHP;
}

// Main function to run the combat
function runCombat(filePath) {
  const { grid, units } = parseInput(filePath);
  let fullRounds = 0;

  while (true) {
    const roundComplete = executeRound(grid, units);
    if (!roundComplete) break;
    fullRounds++;
  }

  const outcome = calculateOutcome(fullRounds, units);
  console.log(`Outcome: ${outcome}`);
}

// Execute the combat with 'input.txt'
runCombat('input.txt');
