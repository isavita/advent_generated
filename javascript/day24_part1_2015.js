const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const lines = data.trim().split('\n');

const packages = lines.map(Number);
const totalWeight = packages.reduce((sum, weight) => sum + weight, 0);

const targetWeight = Math.floor(totalWeight / 3);
let bestQE = Number.MAX_SAFE_INTEGER;
let bestLength = Number.MAX_SAFE_INTEGER;

for (let comb = 1; comb < (1 << packages.length); comb++) {
  let groupWeight = 0, qe = 1, groupLength = 0;
  for (let i = 0; i < packages.length; i++) {
    if (comb & (1 << i)) {
      groupWeight += packages[i];
      qe *= packages[i];
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