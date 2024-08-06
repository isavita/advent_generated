const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

const directions = input.split(',');

let x = 0;
let y = 0;
let z = 0;
let maxDistance = 0;

for (const dir of directions) {
  switch (dir) {
    case 'n':
      y++;
      z--;
      break;
    case 'ne':
      x++;
      z--;
      break;
    case 'se':
      x++;
      y--;
      break;
    case 's':
      y--;
      z++;
      break;
    case 'sw':
      x--;
      z++;
      break;
    case 'nw':
      x--;
      y++;
      break;
  }

  const curDistance = (Math.abs(x) + Math.abs(y) + Math.abs(z)) / 2;
  maxDistance = Math.max(maxDistance, curDistance);
}

console.log(maxDistance);