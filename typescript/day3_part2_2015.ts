
const fs = require('fs');

const directions = fs.readFileSync('input.txt', 'utf8');

let xSanta = 0, ySanta = 0;
let xRobo = 0, yRobo = 0;
let visitedHouses = new Set([`${xSanta},${ySanta}`]);

let isSantaTurn = true;

for (const dir of directions) {
  let x = isSantaTurn ? xSanta : xRobo;
  let y = isSantaTurn ? ySanta : yRobo;

  switch (dir) {
    case '^': y++; break;
    case 'v': y--; break;
    case '>': x++; break;
    case '<': x--; break;
  }

  if (isSantaTurn) {
    xSanta = x;
    ySanta = y;
  } else {
    xRobo = x;
    yRobo = y;
  }

  visitedHouses.add(`${x},${y}`);
  isSantaTurn = !isSantaTurn;
}

console.log(visitedHouses.size);
