
import { readFileSync } from 'fs';

function main() {
  const data = readFileSync('input.txt', 'utf8').split('\n');
  const TOTAL = 100;
  let pos = 50;
  let count = 0;
  for (let line of data) {
    line = line.trim();
    if (!line) continue;
    const dir = line[0];
    const dist = +line.slice(1);
    if (dir === 'L') pos = (pos - dist) % TOTAL;
    else if (dir === 'R') pos = (pos + dist) % TOTAL;
    if (pos < 0) pos += TOTAL;
    if (pos === 0) count++;
  }
  console.log(count);
}

main();
