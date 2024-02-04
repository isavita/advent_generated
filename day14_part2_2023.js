const fs = require('fs');

class Coord {
  constructor(x, y) {
    this.X = x;
    this.Y = y;
  }

  Add(c2) {
    return new Coord(this.X + c2.X, this.Y + c2.Y);
  }

  isInBounds(grid) {
    return 0 <= this.X && this.X < grid.Width && 0 <= this.Y && this.Y < grid.Height;
  }
}

class Grid {
  constructor(width, height, data) {
    this.Width = width;
    this.Height = height;
    this.Data = data;
  }

  toString() {
    let result = '';

    for (let y = 0; y < this.Height; y++) {
      for (let x = 0; x < this.Width; x++) {
        const coord = new Coord(x, y);
        if (this.Data[`${coord.X},${coord.Y}`]) {
          result += this.Data[`${coord.X},${coord.Y}`];
        } else {
          result += '.';
        }
      }
      result += '\n';
    }

    return result;
  }
}

const Empty = '.';
const CubicRock = '#';
const RoundRock = 'O';

const North = new Coord(0, -1);
const West = new Coord(-1, 0);
const South = new Coord(0, 1);
const East = new Coord(1, 0);

function buildGrid(input) {
  const grid = {
    Width: input[0].length,
    Height: input.length,
    Data: {},
  };

  for (let y = 0; y < grid.Height; y++) {
    for (let x = 0; x < grid.Width; x++) {
      if (input[y][x] !== Empty) {
        grid.Data[`${x},${y}`] = input[y][x];
      }
    }
  }

  return new Grid(grid.Width, grid.Height, grid.Data);
}

function shiftSingleRock(grid, coord, dir) {
  if (grid.Data[`${coord.X},${coord.Y}`] === RoundRock) {
    let current = coord;
    let before = coord.Add(dir);

    while (!grid.Data[`${before.X},${before.Y}`] && before.isInBounds(grid)) {
      grid.Data[`${before.X},${before.Y}`] = RoundRock;
      delete grid.Data[`${current.X},${current.Y}`];

      current = before;
      before = before.Add(dir);
    }
  }
}

function shiftRocks(grid, dir) {
  switch (dir) {
    case North:
    case West:
      for (let x = 0; x < grid.Width; x++) {
        for (let y = 0; y < grid.Height; y++) {
          shiftSingleRock(grid, new Coord(x, y), dir);
        }
      }
      break;

    case South:
    case East:
      for (let x = grid.Width - 1; x >= 0; x--) {
        for (let y = grid.Height - 1; y >= 0; y--) {
          shiftSingleRock(grid, new Coord(x, y), dir);
        }
      }
      break;
  }
}

function cycleRocks(grid) {
  shiftRocks(grid, North);
  shiftRocks(grid, West);
  shiftRocks(grid, South);
  shiftRocks(grid, East);
}

function calculateGridKey(grid) {
  let key = 0;

  for (let x = 0; x < grid.Width; x++) {
    for (let y = 0; y < grid.Height; y++) {
      if (grid.Data[`${x},${y}`] === RoundRock) {
        key += x + y * grid.Width;
      }
    }
  }

  return key;
}

function calculateLoad(grid) {
  let load = 0;

  for (let x = 0; x < grid.Width; x++) {
    for (let y = 0; y < grid.Height; y++) {
      if (grid.Data[`${x},${y}`] === RoundRock) {
        load += grid.Height - y;
      }
    }
  }

  return load;
}

function solve(input) {
  const numCycles = 1000000000;

  const grid = buildGrid(input);
  const cache = {};

  for (let i = 0; i < numCycles; i++) {
    const gridKey = calculateGridKey(grid);
    if (cache[gridKey] !== undefined) {
      const iStartCycle = cache[gridKey];
      const remainingCycles = (numCycles - iStartCycle) % (i - iStartCycle);
      for (let j = 0; j < remainingCycles; j++) {
        cycleRocks(grid);
      }
      return calculateLoad(grid);
    }
    cache[gridKey] = i;

    cycleRocks(grid);
  }

  return calculateLoad(grid);
}

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
console.log(solve(input));