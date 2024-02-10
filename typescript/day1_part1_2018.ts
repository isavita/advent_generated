const fs = require('fs');

function readInput() {
  const input = fs.readFileSync('input.txt', 'utf8');
  return input.trim().split('\n');
}

function parseChange(change) {
  const sign = change[0] === '-' ? -1 : 1;
  const num = parseInt(change.slice(1));
  if (isNaN(num)) {
    throw new Error(`invalid frequency change: ${change}`);
  }
  return sign * num;
}

function main() {
  const freqChanges = readInput();
  let freq = 0;
  for (const change of freqChanges) {
    freq += parseChange(change);
  }
  console.log(freq);
}

main();