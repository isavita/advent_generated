const fs = require('fs');

class Coord {
  constructor(public X: number, public Y: number) {}
}

class Grid {
  constructor(public Width: number, public Height: number, public Data: { [key: string]: string }) {}

  toString(empty: string): string {
    let result = '';
    for (let y = 0; y < this.Height; y++) {
      for (let x = 0; x < this.Width; x++) {
        const coord = new Coord(x, y);
        result += this.Data[`${coord.X},${coord.Y}`] || empty;
      }
      result += '\n';
    }
    return result;
  }

  getEmptyRows(): number[] {
    const emptyRows: number[] = [];
    for (let y = 0; y < this.Height; y++) {
      let isEmpty = true;
      for (let x = 0; x < this.Width; x++) {
        if (this.Data[`${x},${y}`]) {
          isEmpty = false;
        }
      }
      if (isEmpty) {
        emptyRows.push(y);
      }
    }
    return emptyRows;
  }

  getEmptyCols(): number[] {
    const emptyCols: number[] = [];
    for (let x = 0; x < this.Width; x++) {
      let isEmpty = true;
      for (let y = 0; y < this.Height; y++) {
        if (this.Data[`${x},${y}`]) {
          isEmpty = false;
        }
      }
      if (isEmpty) {
        emptyCols.push(x);
      }
    }
    return emptyCols;
  }
}

function calculateOffsets(emptyIndexes: number[], bound: number): number[] {
  const offsets = new Array(bound).fill(0);
  for (const idx of emptyIndexes) {
    for (let i = idx + 1; i < offsets.length; i++) {
      offsets[i]++;
    }
  }
  return offsets;
}

function expandGrid(grid: Grid, expansionFactor: number): Grid {
  const emptyCols = grid.getEmptyCols();
  const emptyRows = grid.getEmptyRows();
  const numLinesToAdd = expansionFactor - 1;

  const newGrid = new Grid(
    grid.Width + emptyCols.length * numLinesToAdd,
    grid.Height + emptyRows.length * numLinesToAdd,
    {}
  );

  const dXs = calculateOffsets(emptyCols, grid.Width);
  const dYs = calculateOffsets(emptyRows, grid.Height);

  for (let y = 0; y < grid.Height; y++) {
    for (let x = 0; x < grid.Width; x++) {
      const coord = new Coord(x, y);
      if (grid.Data[`${coord.X},${coord.Y}`]) {
        const newCoord = new Coord(x + dXs[x] * numLinesToAdd, y + dYs[y] * numLinesToAdd);
        newGrid.Data[`${newCoord.X},${newCoord.Y}`] = grid.Data[`${coord.X},${coord.Y}`];
      }
    }
  }

  return newGrid;
}

function abs(x: number): number {
  return x < 0 ? -x : x;
}

function calculateLength(grid: Grid, c1: Coord, c2: Coord): number {
  const dX = abs(c2.X - c1.X);
  const dY = abs(c2.Y - c1.Y);
  return dX + dY;
}

function solve(input: string[]): number {
  const grid = new Grid(input[0].length, input.length, {});
  for (let y = 0; y < input.length; y++) {
    for (let x = 0; x < input[y].length; x++) {
      if (input[y][x] !== '.') {
        grid.Data[`${x},${y}`] = input[y][x];
      }
    }
  }

  const expandedGrid = expandGrid(grid, 2);

  let res = 0;
  const alreadySeen: { [key: string]: boolean } = {};
  for (const coord1 in expandedGrid.Data) {
    for (const coord2 in alreadySeen) {
      const [x1, y1] = coord1.split(',').map(Number);
      const [x2, y2] = coord2.split(',').map(Number);
      const length = calculateLength(expandedGrid, new Coord(x1, y1), new Coord(x2, y2));
      res += length;
    }
    alreadySeen[coord1] = true;
  }

  return res;
}

function readFile(fileName: string): string[] {
  return fs.readFileSync(fileName, 'utf8').trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));