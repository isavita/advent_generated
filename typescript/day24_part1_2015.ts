const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(Number);
const totalWeight = input.reduce((a, b) => a + b, 0);
const targetWeight = totalWeight / 3;
let bestQE = Number.MAX_SAFE_INTEGER;
let bestLength = Number.MAX_SAFE_INTEGER;

for (let comb = 1; comb < (1 << input.length); comb++) {
  let groupWeight = 0;
  let qe = 1;
  let groupLength = 0;
  for (let i = 0; i < input.length; i++) {
    if ((comb & (1 << i)) !== 0) {
      groupWeight += input[i];
      qe *= input[i];
      groupLength++;
    }
  }
  if (groupWeight === targetWeight && groupLength <= bestLength) {
    if (groupLength < bestLength || qe < bestQE) {
      bestLength = groupLength;
      bestQE = qe;
    }
  }
}

console.log(bestQE);