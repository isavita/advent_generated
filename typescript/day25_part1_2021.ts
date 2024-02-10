const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const grid = data.map(line => line.split(''));

console.log(findSafeStep(grid));

function findSafeStep(grid) {
  let step = 0;
  while (true) {
    const eastMoved = moveEast(grid);
    const southMoved = moveSouth(grid);
    step++;

    if (!eastMoved && !southMoved) {
      break;
    }
  }
  return step;
}

function moveEast(grid) {
  let moved = false;
  const height = grid.length;
  const width = grid[0].length;

  const oldPositions = new Array(height).fill(null).map(() => new Array(width).fill(null));
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      if (grid[y][x] === '>') {
        const nextX = (x + 1) % width;
        if (grid[y][nextX] === '.') {
          oldPositions[y][x] = '.';
          grid[y][nextX] = '>';
          x++;
          moved = true;
        }
      }
    }
  }
  freeEmptyPositions(grid, oldPositions);

  return moved;
}

function moveSouth(grid) {
  let moved = false;
  const height = grid.length;
  const width = grid[0].length;

  const oldPositions = new Array(height).fill(null).map(() => new Array(width).fill(null));
  for (let x = 0; x < width; x++) {
    for (let y = 0; y < height; y++) {
      if (grid[y][x] === 'v') {
        const nextY = (y + 1) % height;
        if (grid[nextY][x] === '.') {
          oldPositions[y][x] = '.';
          grid[nextY][x] = 'v';
          y++;
          moved = true;
        }
      }
    }
  }
  freeEmptyPositions(grid, oldPositions);

  return moved;
}

function freeEmptyPositions(grid, oldPostion) {
  for (let y = 0; y < grid.length; y++) {
    for (let x = 0; x < grid[0].length; x++) {
      if (oldPostion[y][x] === '.') {
        grid[y][x] = '.';
      }
    }
  }
}