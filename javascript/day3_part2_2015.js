const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const directions = data.trim();

const visitedHouses = new Set();
let [xSanta, ySanta] = [0, 0];
let [xRobo, yRobo] = [0, 0];
let isSantaTurn = true;

visitedHouses.add(`${xSanta},${ySanta}`);

for (const dir of directions) {
  let [x, y] = isSantaTurn ? [xSanta, ySanta] : [xRobo, yRobo];

  switch (dir) {
    case '^':
      y++;
      break;
    case 'v':
      y--;
      break;
    case '>':
      x++;
      break;
    case '<':
      x--;
      break;
  }

  isSantaTurn ? ([xSanta, ySanta] = [x, y]) : ([xRobo, yRobo] = [x, y]);
  visitedHouses.add(`${x},${y}`);
  isSantaTurn = !isSantaTurn;
}

console.log(visitedHouses.size);