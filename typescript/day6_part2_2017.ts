const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const banks = data.trim().split(/\s+/).map(Number);

const seen = new Map();
let cycles = 0;

while (true) {
  const state = JSON.stringify(banks);
  if (seen.has(state)) {
    console.log(cycles - seen.get(state));
    break;
  }
  seen.set(state, cycles);

  const maxIndex = banks.indexOf(Math.max(...banks));
  const blocks = banks[maxIndex];
  banks[maxIndex] = 0;
  for (let i = 1; i <= blocks; i++) {
    banks[(maxIndex + i) % banks.length]++;
  }

  cycles++;
}