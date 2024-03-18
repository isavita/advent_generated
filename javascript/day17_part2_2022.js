const fs = require('fs');

const rockstr = `####

 # 
###
 # 

  #
  #
###

#
#
#
#

##
##`;

function readAll(path) {
  return fs.readFileSync(path, 'utf-8').trim();
}

function getRocks() {
  const rocks = [];
  for (const rock of rockstr.split('\n\n')) {
    const rockMap = new Map();
    const lines = rock.split('\n');
    for (let y = 0; y < lines.length; y++) {
      for (let x = 0; x < lines[y].length; x++) {
        if (lines[y][x] === '#') {
          rockMap.set(`${x},${lines.length - 1 - y}`, true);
        }
      }
    }
    rocks.push(rockMap);
  }
  return rocks;
}

function collision(grid, rock, pos) {
  for (const [key] of rock) {
    const [x, y] = key.split(',').map(Number);
    const newX = x + pos.x;
    const newY = y + pos.y;
    if (newX < 0 || newX > 6 || grid.has(`${newX},${newY}`)) {
      return true;
    }
  }
  return false;
}

function main() {
  const jetPattern = readAll('input.txt').split('');
  const rocks = getRocks();
  const grid = new Set();
  for (let x = 0; x < 7; x++) {
    grid.add(`${x},0`);
  }
  let floor = 0;
  let j = 0;
  const repeat = new Map();

  for (let i = 0, curr = 0; ; i++, curr = (curr + 1) % rocks.length) {
    const key = `${curr},${j}`;
    if (repeat.has(key)) {
      const [previ, prevFloor] = repeat.get(key);
      if ((1000000000000 - i) % (i - previ) === 0) {
        console.log(floor + Math.floor((1000000000000 - i) / (i - previ) * (floor - prevFloor)));
        break;
      }
    }
    repeat.set(key, [i, floor]);
    const currRock = rocks[curr];
    let pos = { x: 2, y: floor + 4 };
    while (true) {
      const jet = jetPattern[j];
      j = (j + 1) % jetPattern.length;
      const dir = getDir(jet);
      pos = { x: pos.x + dir.x, y: pos.y + dir.y };
      if (collision(grid, currRock, pos)) {
        pos = { x: pos.x - dir.x, y: pos.y - dir.y };
      }
      pos = { x: pos.x, y: pos.y - 1 };
      if (collision(grid, currRock, pos)) {
        pos = { x: pos.x, y: pos.y + 1 };
        for (const [key] of currRock) {
          const [x, y] = key.split(',').map(Number);
          grid.add(`${x + pos.x},${y + pos.y}`);
          if (y + pos.y > floor) {
            floor = y + pos.y;
          }
        }
        break;
      }
    }
  }
}

function getDir(b) {
  switch (b) {
    case '<':
      return { x: -1, y: 0 };
    case '>':
      return { x: 1, y: 0 };
    default:
      throw new Error(`Invalid direction: ${b}`);
  }
}

main();