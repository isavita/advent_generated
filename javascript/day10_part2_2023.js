const fs = require('fs');

class Coord {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  add(other) {
    return new Coord(this.x + other.x, this.y + other.y);
  }

  subtract(other) {
    return new Coord(this.x - other.x, this.y - other.y);
  }

  opposite() {
    return new Coord(-this.x, -this.y);
  }

  toString() {
    return `${this.x},${this.y}`;
  }

  equals(other) {
    return this.x === other.x && this.y === other.y;
  }

  static fromString(str) {
    const [x, y] = str.split(',').map(Number);
    return new Coord(x, y);
  }

  getPipeFromNeighbors(grid) {
    const pipe = new Set();

    const possibleNeighbors = {
      [Top.toString()]: this.add(Top),
      [Right.toString()]: this.add(Right),
      [Bottom.toString()]: this.add(Bottom),
      [Left.toString()]: this.add(Left),
    };

    for (const [dir, neighborCoord] of Object.entries(possibleNeighbors)) {
      const neighborPipe = getPipeFromTile(grid.data.get(neighborCoord.toString()));
      if (neighborPipe.has(Coord.fromString(dir).opposite().toString())) {
        pipe.add(dir);
      }
    }

    return pipe;
  }

  pathFinding(grid) {
    const path = [this];
    const startPipe = this.getPipeFromNeighbors(grid);

    let previousDir = Undefined;
    let current = Undefined;
    for (const dir of startPipe) {
      previousDir = Coord.fromString(dir);
      current = this.add(previousDir);
    }

    while (!current.equals(this)) {
      path.push(current);
      const currentPipe = getPipeFromTile(grid.data.get(current.toString()));
      for (const dir of currentPipe) {
        if (dir !== previousDir.opposite().toString()) {
          previousDir = Coord.fromString(dir);
          current = current.add(previousDir);
          break;
        }
      }
    }

    return path;
  }

  isInside(grid, empty) {
    if (grid.data.has(this.toString())) {
      return false;
    }

    let startPipe = empty;
    let numPipeOnLeft = 0;
    for (let x = 0; x < this.x; x++) {
      const coord = new Coord(x, this.y);
      const v = grid.data.get(coord.toString());

      switch (v) {
        case '|':
          numPipeOnLeft++;
          break;
        case 'L':
          startPipe = 'L';
          break;
        case 'F':
          startPipe = 'F';
          break;
        case 'J':
          if (startPipe === 'F') {
            startPipe = empty;
            numPipeOnLeft++;
          } else if (v === 'L') {
            startPipe = '.';
          }
          break;
        case '7':
          if (startPipe === 'L') {
            startPipe = '.';
            numPipeOnLeft++;
          } else if (startPipe === 'F') {
            startPipe = '.';
          }
          break;
      }
    }

    return numPipeOnLeft % 2 === 1;
  }
}

class Grid {
  constructor(width, height, data) {
    this.width = width;
    this.height = height;
    this.data = data;
  }

  toString() {
    const pipesRepres = {
      '.': ' ',
      'S': 'S',
      '|': '║',
      '-': '═',
      'J': '╝',
      'L': '╚',
      '7': '╗',
      'F': '╔',
      'X': 'X',
    };

    let res = '';

    for (let y = 0; y < this.height; y++) {
      for (let x = 0; x < this.width; x++) {
        const coord = new Coord(x, y);
        res += pipesRepres[this.data.get(coord.toString()) || '.'];
      }
      res += '\n';
    }

    return res;
  }
}

const Undefined = new Coord(0, 0);
const Top = new Coord(0, -1);
const Right = new Coord(1, 0);
const Bottom = new Coord(0, 1);
const Left = new Coord(-1, 0);

const VerticalPipe = new Set([Top.toString(), Bottom.toString()]);
const HorizontalPipe = new Set([Left.toString(), Right.toString()]);
const TopLeftCornerPipe = new Set([Top.toString(), Left.toString()]);
const TopRightCornerPipe = new Set([Top.toString(), Right.toString()]);
const BottomLeftCornerPipe = new Set([Bottom.toString(), Left.toString()]);
const BottomRightCornerPipe = new Set([Bottom.toString(), Right.toString()]);

const TileToPipe = {
  '|': VerticalPipe,
  '-': HorizontalPipe,
  'J': TopLeftCornerPipe,
  'L': TopRightCornerPipe,
  '7': BottomLeftCornerPipe,
  'F': BottomRightCornerPipe,
};

function getPipeFromTile(tile) {
  return TileToPipe[tile] || new Set();
}

function getTileFromPipe(pipe) {
  for (const [tile, associatedPipe] of Object.entries(TileToPipe)) {
    if (isEqualPipe(pipe, associatedPipe)) {
      return tile;
    }
  }

  return '.';
}

function isEqualPipe(pipe1, pipe2) {
  if (pipe1.size !== pipe2.size) {
    return false;
  }

  for (const dir of pipe1) {
    if (!pipe2.has(dir)) {
      return false;
    }
  }

  return true;
}

function buildGrid(input) {
  const width = input[0].length;
  const height = input.length;
  const data = new Map();

  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const char = input[y][x];
      if (char !== '.') {
        data.set(new Coord(x, y).toString(), char);
      }
    }
  }

  return new Grid(width, height, data);
}

function findStart(grid) {
  for (const [coord, value] of grid.data.entries()) {
    if (value === 'S') {
      return Coord.fromString(coord);
    }
  }
  return Undefined;
}

function getPathGrid(grid, path, empty) {
  const data = new Map();

  for (const coord of path) {
    data.set(coord.toString(), grid.data.get(coord.toString()));
  }

  const start = path[0];
  data.set(start.toString(), getTileFromPipe(start.getPipeFromNeighbors(grid)));

  return new Grid(grid.width, grid.height, data);
}

function solve(input) {
  const grid = buildGrid(input);
  const start = findStart(grid);
  const path = start.pathFinding(grid);
  const pathGrid = getPathGrid(grid, path, '.');

  let cnt = 0;
  for (let y = 0; y < grid.height; y++) {
    for (let x = 0; x < grid.width; x++) {
      const c = new Coord(x, y);
      if (c.isInside(pathGrid, '.')) {
        cnt++;
      }
    }
  }

  return cnt;
}

function readFile(fileName) {
  const file = fs.readFileSync(fileName, 'utf-8');
  return file.trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));