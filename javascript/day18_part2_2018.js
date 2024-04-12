const fs = require('fs');

const Open = '.';
const Trees = '|';
const Lumberyard = '#';

function readInput(filename) {
  const data = fs.readFileSync(filename, 'utf8');
  return data.trim().split('\n').map(row => row.split(''));
}

function transform(grid) {
  const newGrid = grid.map(row => [...row]);
  for (let i = 0; i < grid.length; i++) {
    for (let j = 0; j < grid[i].length; j++) {
      newGrid[i][j] = nextAcreState(grid, i, j);
    }
  }
  return newGrid;
}

function nextAcreState(grid, i, j) {
  switch (grid[i][j]) {
    case Open:
      return countAdjacent(grid, i, j, Trees) >= 3 ? Trees : Open;
    case Trees:
      return countAdjacent(grid, i, j, Lumberyard) >= 3 ? Lumberyard : Trees;
    case Lumberyard:
      return countAdjacent(grid, i, j, Lumberyard) >= 1 && countAdjacent(grid, i, j, Trees) >= 1 ? Lumberyard : Open;
    default:
      return grid[i][j];
  }
}

function countAdjacent(grid, i, j, acreType) {
  let count = 0;
  for (let x = -1; x <= 1; x++) {
    for (let y = -1; y <= 1; y++) {
      if (x === 0 && y === 0) continue;
      if (i + x >= 0 && i + x < grid.length && j + y >= 0 && j + y < grid[i].length && grid[i + x][j + y] === acreType) {
        count++;
      }
    }
  }
  return count;
}

function countResources(grid) {
  let wooded = 0;
  let lumberyards = 0;
  for (let i = 0; i < grid.length; i++) {
    for (let j = 0; j < grid[i].length; j++) {
      if (grid[i][j] === Trees) wooded++;
      else if (grid[i][j] === Lumberyard) lumberyards++;
    }
  }
  return [wooded, lumberyards];
}

function gridToString(grid) {
  return grid.map(row => row.join('')).join('\n');
}

function main() {
  let grid = readInput('input.txt');
  const seenStates = new Map();
  let cycleStart, cycleLength;
  let minute = 0;
  while (true) {
    const state = gridToString(grid);
    if (seenStates.has(state)) {
      cycleStart = seenStates.get(state);
      cycleLength = minute - cycleStart;
      break;
    }
    seenStates.set(state, minute);
    grid = transform(grid);
    minute++;
  }

  const remainingMinutes = (1000000000 - cycleStart) % cycleLength;
  for (let i = 0; i < remainingMinutes; i++) {
    grid = transform(grid);
  }

  const [wooded, lumberyards] = countResources(grid);
  console.log(wooded * lumberyards);
}

main();