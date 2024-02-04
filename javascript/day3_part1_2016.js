const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let count = 0;

for (const line of input) {
  const sides = line.trim().split(/\s+/).map(Number);
  sides.sort((a, b) => a - b);
  if (sides[0] + sides[1] > sides[2]) {
    count++;
  }
}

console.log(count);