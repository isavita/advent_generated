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

const N = 0, E = 1, S = 2, W = 3;
const point = {[N]: {x: 0, y: 1}, [E]: {x: 1, y: 0}, [S]: {x: 0, y: -1}, [W]: {x: -1, y: 0}};
const fromByte = {'<': W, '>': E};

function main() {
  const jetPattern = fs.readFileSync('input.txt', 'utf8').trim().split('');

  const rocks = getRocks();
  const grid = new Set();
  for (let x = 0; x < 7; x++) {
    grid.add(`${x},0`);
  }
  let floor = 0, j = 0;

  for (let i = 0, curr = 0; i < 2022; i++, curr = (curr + 1) % rocks.length) {
    const currRock = rocks[curr];
    let pos = {x: 2, y: floor + 4};
    while (true) {
      const jet = jetPattern[j];
      j = (j + 1) % jetPattern.length;
      const newPos = {x: pos.x + point[fromByte[jet]].x, y: pos.y + point[fromByte[jet]].y};
      if (!collision(grid, currRock, newPos)) {
        pos = newPos;
      }
      const downPos = {x: pos.x + point[S].x, y: pos.y + point[S].y};
      if (collision(grid, currRock, downPos)) {
        for (const p of currRock) {
          const rockPos = `${p.x + pos.x},${p.y + pos.y}`;
          grid.add(rockPos);
          floor = Math.max(floor, p.y + pos.y);
        }
        break;
      }
      pos = downPos;
    }
  }
  console.log(floor);
}

function collision(grid, rock, pos) {
  for (const p of rock) {
    const rockPos = `${p.x + pos.x},${p.y + pos.y}`;
    if (grid.has(rockPos) || p.x + pos.x < 0 || p.x + pos.x > 6) {
      return true;
    }
  }
  return false;
}

function getRocks() {
  const rocks = [];
  for (const rock of rockstr.split('\n\n')) {
    const currRock = [];
    const lines = rock.split('\n');
    for (let y = 0; y < lines.length; y++) {
      const line = lines[y];
      for (let x = 0; x < line.length; x++) {
        if (line[x] === '#') {
          currRock.push({x, y: lines.length - 1 - y});
        }
      }
    }
    rocks.push(currRock);
  }
  return rocks;
}

main();