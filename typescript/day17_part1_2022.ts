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

const jetPattern = fs.readFileSync('input.txt', 'utf8').trim().split('');

const rocks = getRocks();
const grid: { [key: string]: boolean } = {};
for (let x = 0; x < 7; x++) {
  grid[`${x},0`] = true;
}

let floor = 0;
let j = 0;
const repeat: { [key: string]: [number, number] } = {};

for (let i = 0; i < 2022; i++) {
  const curr = i % rocks.length;
  const key = `${curr},${j}`;
  repeat[key] = [i, floor];
  const currRock = rocks[curr];
  let pos = { x: 2, y: floor + 4 };
  while (true) {
    const jet = jetPattern[j];
    j = (j + 1) % jetPattern.length;
    pos = add(pos, dirFromByte(jet));
    if (collision(grid, currRock, pos)) {
      pos = add(pos, reverse(dirFromByte(jet)));
    }
    pos = add(pos, { x: 0, y: -1 });
    if (collision(grid, currRock, pos)) {
      pos = add(pos, { x: 0, y: 1 });
      for (const p of Object.keys(currRock)) {
        const key = `${parseInt(p.split(',')[0]) + pos.x},${parseInt(p.split(',')[1]) + pos.y}`;
        grid[key] = true;
        if (parseInt(p.split(',')[1]) + pos.y > floor) {
          floor = parseInt(p.split(',')[1]) + pos.y;
        }
      }
      break;
    }
  }
}

console.log(floor);

function collision(grid: { [key: string]: boolean }, rock: { [key: string]: boolean }, pos: { x: number; y: number }) {
  for (const p of Object.keys(rock)) {
    const key = `${parseInt(p.split(',')[0]) + pos.x},${parseInt(p.split(',')[1]) + pos.y}`;
    if (grid[key] || parseInt(p.split(',')[0]) + pos.x < 0 || parseInt(p.split(',')[0]) + pos.x > 6) {
      return true;
    }
  }
  return false;
}

function getRocks() {
  const rocks = [];
  const rockLines = rockstr.split('\n\n');
  for (const rock of rockLines) {
    const rockMap: { [key: string]: boolean } = {};
    const lines = rock.split('\n');
    for (let y = 0; y < lines.length; y++) {
      for (let x = 0; x < lines[y].length; x++) {
        if (lines[y][x] === '#') {
          rockMap[`${x},${lines.length - 1 - y}`] = true;
        }
      }
    }
    rocks.push(rockMap);
  }
  return rocks;
}

function dirFromByte(b: string) {
  switch (b) {
    case '>':
      return { x: 1, y: 0 };
    case '<':
      return { x: -1, y: 0 };
    default:
      throw new Error(`Unknown direction: ${b}`);
  }
}

function reverse(dir: { x: number; y: number }) {
  return { x: -dir.x, y: -dir.y };
}

function add(a: { x: number; y: number }, b: { x: number; y: number }) {
  return { x: a.x + b.x, y: a.y + b.y };
}