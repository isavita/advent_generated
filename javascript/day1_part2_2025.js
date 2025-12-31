
// ------------- main.js -------------
const fs = require('fs');
const INPUT = fs.readFileSync('input.txt', 'utf8').trimEnd().split('\n');

const DIAL = 100;
let pos = 50;                     // current position
let total = 0n;                   // use BigInt for unlimited count

function floorDiv(a, b) {        // floor division for negative numbers
  return Math.floor(a / b);
}

for (let raw of INPUT) {
  const line = raw.trim();
  if (!line) continue;

  const dir = line[0];
  const amt = Number(line.slice(1));

  if (dir === 'R') {
    total += BigInt(Math.floor((pos + amt) / DIAL));
    pos = (pos + amt) % DIAL;
  } else if (dir === 'L') {
    const before = floorDiv(pos - 1, DIAL);
    const after  = floorDiv(pos - amt - 1, DIAL);
    total += BigInt(before - after);
    pos = (pos - amt) % DIAL;
    if (pos < 0) pos += DIAL;
  } else {
    throw new Error(`Unknown direction '${dir}'`);
  }
}

console.log(`The password is: ${total}`);
