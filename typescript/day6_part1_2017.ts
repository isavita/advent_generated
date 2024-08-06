const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split(/\s+/).map(Number);

const seen = new Set();
let cycles = 0;

while (true) {
  const state = JSON.stringify(input);
  if (seen.has(state)) break;
  seen.add(state);

  const maxIndex = input.indexOf(Math.max(...input));
  const blocks = input[maxIndex];
  input[maxIndex] = 0;
  for (let i = 1; i <= blocks; i++) {
    input[(maxIndex + i) % input.length]++;
  }

  cycles++;
}

console.log(`It takes ${cycles} redistribution cycles to reach a repeated configuration.`);