const fs = require('fs');

const Side = 5;
const Square = Side * Side;

function parse() {
  const res = new Array(Square).fill(false);

  const data = fs.readFileSync('input.txt', 'utf8').split('\n');
  for (let row = 0; row < Side; row++) {
    const line = data[row];
    for (let col = 0; col < Side; col++) {
      if (line[col] === '#') {
        res[row * Side + col] = true;
      }
    }
  }

  return res;
}

function next1(grid) {
  const newGrid = new Array(Square).fill(false);

  for (let i = 0; i < Square; i++) {
    const row = Math.floor(i / Side);
    const col = i % Side;
    let neighbours = 0;

    if (row > 0 && grid[i - Side]) {
      neighbours++;
    }
    if (row < Side - 1 && grid[i + Side]) {
      neighbours++;
    }
    if (col > 0 && grid[i - 1]) {
      neighbours++;
    }
    if (col < Side - 1 && grid[i + 1]) {
      neighbours++;
    }

    if (grid[i] && neighbours !== 1) {
      newGrid[i] = false;
      continue;
    }

    if (!grid[i] && (neighbours === 1 || neighbours === 2)) {
      newGrid[i] = true;
      continue;
    }

    newGrid[i] = grid[i];
  }
  return newGrid;
}

function biodiversity(grid) {
  let bio = 0;
  for (let i = 0; i < Square; i++) {
    if (grid[i]) {
      bio += 1 << i;
    }
  }
  return bio;
}

const appeared = new Set();
let grid = parse();
appeared.add(grid.toString());

while (true) {
  grid = next1(grid);
  if (appeared.has(grid.toString())) {
    console.log(biodiversity(grid));
    break;
  }
  appeared.add(grid.toString());
}