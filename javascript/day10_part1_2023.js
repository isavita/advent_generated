const fs = require('fs');

class Coord {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  add(c) {
    return new Coord(this.x + c.x, this.y + c.y);
  }

  subtract(c) {
    return new Coord(this.x - c.x, this.y - c.y);
  }

  opposite() {
    return new Coord(-this.x, -this.y);
  }

  equals(other) {
    return this.x === other.x && this.y === other.y;
  }

  toString() {
    return `${this.x},${this.y}`;
  }
}

const Undefined = new Coord(0, 0);
const Top = new Coord(0, -1);
const Right = new Coord(1, 0);
const Bottom = new Coord(0, 1);
const Left = new Coord(-1, 0);

const Empty = '.';
const Start = 'S';
const Vertical = '|';
const Horizontal = '-';
const TopLeftCorner = 'J';
const TopRightCorner = 'L';
const BottomLeftCorner = '7';
const BottomRightCorner = 'F';
const Enclosed = 'X';

const TileToPipe = new Map([
  [Vertical, { [Top.toString()]: true, [Bottom.toString()]: true }],
  [Horizontal, { [Left.toString()]: true, [Right.toString()]: true }],
  [TopLeftCorner, { [Top.toString()]: true, [Left.toString()]: true }],
  [TopRightCorner, { [Top.toString()]: true, [Right.toString()]: true }],
  [BottomLeftCorner, { [Bottom.toString()]: true, [Left.toString()]: true }],
  [BottomRightCorner, { [Bottom.toString()]: true, [Right.toString()]: true }],
]);

function getPipeFromTile(tile) {
  return TileToPipe.get(tile) || {};
}

function getTileFromPipe(pipe) {
  for (const [tile, associatedPipe] of TileToPipe) {
    if (isEqualPipe(pipe, associatedPipe)) {
      return tile;
    }
  }
  return Empty;
}

function isEqualPipe(pipe1, pipe2) {
  if (Object.keys(pipe1).length !== Object.keys(pipe2).length) {
    return false;
  }

  for (const dir in pipe1) {
    if (!(dir in pipe2)) {
      return false;
    }
  }

  return true;
}

function buildGrid(input) {
  const grid = {
    width: input[0].length,
    height: input.length,
    data: {},
  };

  for (let y = 0; y < grid.height; y++) {
    for (let x = 0; x < grid.width; x++) {
      const char = input[y][x];
      if (char !== Empty) {
        grid.data[new Coord(x, y).toString()] = char;
      }
    }
  }

  return grid;
}

function toString(grid) {
  const pipesRepres = new Map([
    [Empty, ' '],
    [Start, 'S'],
    [Vertical, '║'],
    [Horizontal, '═'],
    [TopLeftCorner, '╝'],
    [TopRightCorner, '╚'],
    [BottomLeftCorner, '╗'],
    [BottomRightCorner, '╔'],
    [Enclosed, 'X'],
  ]);

  let res = '';

  for (let y = 0; y < grid.height; y++) {
    for (let x = 0; x < grid.width; x++) {
      const coord = new Coord(x, y).toString();
      res += pipesRepres.get(grid.data[coord] || Empty);
    }
    res += '\n';
  }

  return res;
}

function findStart(grid) {
  for (const coord in grid.data) {
    if (grid.data[coord] === Start) {
      return new Coord(...coord.split(',').map(Number));
    }
  }
  return Undefined;
}

function getPipeFromNeighbors(coord, grid) {
  const pipe = {};

  const possibleNeighbors = new Map([
    [Top, coord.add(Top)],
    [Right, coord.add(Right)],
    [Bottom, coord.add(Bottom)],
    [Left, coord.add(Left)],
  ]);

  for (const [dir, neighborCoord] of possibleNeighbors) {
    const neighborPipe = getPipeFromTile(grid.data[neighborCoord.toString()]);
    if (neighborPipe[dir.opposite().toString()]) {
      pipe[dir.toString()] = true;
    }
  }

  return pipe;
}

function pathFinding(start, grid) {
  const path = [start];
  const startPipe = getPipeFromNeighbors(start, grid);

  let previousDir = undefined;
  let current = start;
  for (const dir in startPipe) {
    previousDir = new Coord(...dir.split(',').map(Number));
    current = start.add(previousDir);
  }

  while (!current.equals(start)) {
    path.push(current);
    const currentPipe = getPipeFromTile(grid.data[current.toString()]);
    let foundNextDir = false;
    for (const dir in currentPipe) {
      if (new Coord(...dir.split(',').map(Number)).opposite().toString() !== previousDir.toString()) {
        previousDir = new Coord(...dir.split(',').map(Number));
        current = current.add(previousDir);
        foundNextDir = true;
        break;
      }
    }
    if (!foundNextDir) {
      break;
    }
  }

  return path;
}

function getPathGrid(grid, path, empty) {
  const newGrid = {
    width: grid.width,
    height: grid.height,
    data: {},
  };

  for (const coord of path) {
    newGrid.data[coord.toString()] = grid.data[coord.toString()];
  }

  const start = path[0];
  newGrid.data[start.toString()] = getTileFromPipe(getPipeFromNeighbors(start, grid));

  return newGrid;
}

function isInside(coord, grid, empty) {
  if (grid.data[coord.toString()]) {
    return false;
  }

  let startPipe = empty;
  let numPipeOnLeft = 0;
  for (let x = 0; x < coord.x; x++) {
    const c = new Coord(x, coord.y);
    const v = grid.data[c.toString()];

    switch (v) {
      case Vertical:
        numPipeOnLeft++;
        break;
      case TopRightCorner:
        startPipe = TopRightCorner;
        break;
      case BottomRightCorner:
        startPipe = BottomRightCorner;
        break;
      case TopLeftCorner:
        if (startPipe === BottomRightCorner) {
          startPipe = empty;
          numPipeOnLeft++;
        } else if (v === TopRightCorner) {
          startPipe = Empty;
        }
        break;
      case BottomLeftCorner:
        if (startPipe === TopRightCorner) {
          startPipe = Empty;
          numPipeOnLeft++;
        } else if (startPipe === BottomRightCorner) {
          startPipe = Empty;
        }
        break;
    }
  }

  return numPipeOnLeft % 2 === 1;
}

function solve(input) {
  const grid = buildGrid(input);
  const start = findStart(grid);
  const path = pathFinding(start, grid);
  const numPipesVisited = path.length;
  const maxLength = Math.floor(numPipesVisited / 2);
  return maxLength;
}

function readFile(fileName) {
  return fs.readFileSync(fileName, 'utf-8').trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));