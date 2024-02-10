const fs = require('fs');

class Coord {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  add(c2) {
    return new Coord(this.x + c2.x, this.y + c2.y);
  }

  isInBounds(grid) {
    return 0 <= this.x && this.x < grid.width && 0 <= this.y && this.y < grid.height;
  }
}

class Grid {
  constructor(width, height, data) {
    this.width = width;
    this.height = height;
    this.data = data;
  }

  toString() {
    let result = '';

    for (let y = 0; y < this.height; y++) {
      for (let x = 0; x < this.width; x++) {
        const coord = new Coord(x, y);
        if (this.data[JSON.stringify(coord)]) {
          result += this.data[JSON.stringify(coord)];
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
  const data = {};

  for (let y = 0; y < input.length; y++) {
    for (let x = 0; x < input[y].length; x++) {
      if (input[y][x] !== Empty) {
        const coord = new Coord(x, y);
        data[JSON.stringify(coord)] = input[y][x];
      }
    }
  }

  return new Grid(input[0].length, input.length, data);
}

function shiftSingleRock(grid, coord, dir) {
  if (grid.data[JSON.stringify(coord)] === RoundRock) {
    let current = coord;
    let before = current.add(dir);

    while (!grid.data[JSON.stringify(before)] && before.isInBounds(grid)) {
      grid.data[JSON.stringify(before)] = RoundRock;
      delete grid.data[JSON.stringify(current)];

      current = before;
      before = current.add(dir);
    }
  }
}

function shiftRocks(grid, dir) {
  switch (dir) {
    case North:
    case West:
      for (let x = 0; x < grid.width; x++) {
        for (let y = 0; y < grid.height; y++) {
          shiftSingleRock(grid, new Coord(x, y), dir);
        }
      }
      break;

    case South:
    case East:
      for (let x = grid.width - 1; x >= 0; x--) {
        for (let y = grid.height - 1; y >= 0; y--) {
          shiftSingleRock(grid, new Coord(x, y), dir);
        }
      }
      break;
  }
}

function calculateLoad(grid) {
  let load = 0;

  for (let x = 0; x < grid.width; x++) {
    for (let y = 0; y < grid.height; y++) {
      const coord = new Coord(x, y);
      if (grid.data[JSON.stringify(coord)] === RoundRock) {
        load += grid.height - y;
      }
    }
  }

  return load;
}

function solve(input) {
  const grid = buildGrid(input);
  shiftRocks(grid, North);

  return calculateLoad(grid);
}

function readFile(fileName) {
  const data = fs.readFileSync(fileName, 'utf8');
  return data.trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));