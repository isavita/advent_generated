
import { readFileSync } from 'fs';

function main() {
  const lines = readFileSync('input.txt', 'utf8')
    .split('\n')
    .filter(l => l.trim().length > 0);
  if (lines.length === 0) return;

  const rows = lines.length;
  const cols = lines[0].length;

  let startR = -1,
    startC = -1;
  for (let r = 0; r < rows; r++) {
    const c = lines[r].indexOf('S');
    if (c !== -1) {
      startR = r;
      startC = c;
      break;
    }
  }
  if (startR === -1) {
    console.log('0');
    return;
  }

  let dp = new Array<bigint>(cols).fill(0n);
  dp[startC] = 1n;
  let total = 0n;

  for (let r = startR; r < rows; r++) {
    const next = new Array<bigint>(cols).fill(0n);
    for (let c = 0; c < cols; c++) {
      const cur = dp[c];
      if (cur === 0n) continue;
      const nr = r + 1;
      if (nr >= rows) {
        total += cur;
        continue;
      }
      const cell = lines[nr][c];
      if (cell === '^') {
        if (c - 1 < 0) total += cur;
        else next[c - 1] += cur;
        if (c + 1 >= cols) total += cur;
        else next[c + 1] += cur;
      } else {
        next[c] += cur;
      }
    }
    dp = next;
  }

  console.log(total.toString());
}

main();
