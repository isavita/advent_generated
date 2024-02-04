const fs = require('fs');

const Side = 5;
const Square = Side * Side;

function parse() {
  const res = new Array(Square).fill(false);

  const input = fs.readFileSync('input.txt', 'utf8').split('\n');

  for (let row = 0; row < Side; row++) {
    const line = input[row];
    for (let col = 0; col < Side; col++) {
      if (line[col] === '#') {
        res[row * Side + col] = true;
      }
    }
  }
  return res;
}

function next2(space) {
  const newSpace = {};

  const [minLevel, maxLevel] = minMaxLevel(space);

  for (let level = minLevel - 1; level <= maxLevel + 1; level++) {
    newSpace[level] = new Array(Square).fill(false);

    for (let cell = 0; cell < Square; cell++) {
      if (cell === 12) {
        continue;
      }

      const row = Math.floor(cell / Side);
      const col = cell % Side;
      let neighbours = 0;

      if (row === 0) {
        if (infested(space, level - 1, 7)) {
          neighbours++;
        }
      }

      if (col === 0) {
        if (infested(space, level - 1, 11)) {
          neighbours++;
        }
      }

      if (col === 4) {
        if (infested(space, level - 1, 13)) {
          neighbours++;
        }
      }

      if (row === 4) {
        if (infested(space, level - 1, 17)) {
          neighbours++;
        }
      }

      if (cell === 7) {
        for (let i = 0; i < Side; i++) {
          if (infested(space, level + 1, i)) {
            neighbours++;
          }
        }
      }

      if (cell === 11) {
        for (let i = 0; i < Side; i++) {
          if (infested(space, level + 1, 5 * i)) {
            neighbours++;
          }
        }
      }

      if (cell === 13) {
        for (let i = 0; i < Side; i++) {
          if (infested(space, level + 1, 5 * i + Side - 1)) {
            neighbours++;
          }
        }
      }

      if (cell === 17) {
        for (let i = 0; i < Side; i++) {
          if (infested(space, level + 1, (Side - 1) * Side + i)) {
            neighbours++;
          }
        }
      }

      if (row > 0 && cell !== 17) {
        if (infested(space, level, cell - Side)) {
          neighbours++;
        }
      }

      if (col > 0 && cell !== 13) {
        if (infested(space, level, cell - 1)) {
          neighbours++;
        }
      }

      if (col < Side - 1 && cell !== 11) {
        if (infested(space, level, cell + 1)) {
          neighbours++;
        }
      }

      if (row < Side - 1 && cell !== 7) {
        if (infested(space, level, cell + Side)) {
          neighbours++;
        }
      }

      if (infested(space, level, cell) && neighbours !== 1) {
        newSpace[level][cell] = false;
        continue;
      }

      if (!infested(space, level, cell) && (neighbours === 1 || neighbours === 2)) {
        newSpace[level][cell] = true;
        continue;
      }

      newSpace[level][cell] = infested(space, level, cell);
    }
  }

  clean(newSpace);

  return newSpace;
}

function clean(space) {
  const [min, max] = minMaxLevel(space);

  let countMin = 0;
  let countMax = 0;
  for (let cell = 0; cell < Square; cell++) {
    if (space[min][cell]) {
      countMin++;
    }
    if (space[max][cell]) {
      countMax++;
    }
  }
  if (countMin === 0) {
    delete space[min];
  }
  if (countMax === 0) {
    delete space[max];
  }
}

function infested(space, level, cell) {
  if (!space[level]) {
    return false;
  }
  return space[level][cell];
}

function minMaxLevel(space) {
  let min = 999999;
  let max = -999999;
  for (let level in space) {
    level = parseInt(level);
    if (level < min) {
      min = level;
    }
    if (level > max) {
      max = level;
    }
  }
  return [min, max];
}

const input = parse();

let space = {
  0: input,
};

for (let i = 0; i < 200; i++) {
  space = next2(space);
}

let count = 0;
for (let grid of Object.values(space)) {
  for (let i = 0; i < Square; i++) {
    if (grid[i]) {
      count++;
    }
  }
}
console.log(count);