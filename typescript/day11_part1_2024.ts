
import * as fs from 'fs';

const data = fs.readFileSync('input.txt', 'utf-8');
let stones = data.trim().split(/\s+/);

for (let i = 0; i < 25; i++) {
  const next: string[] = [];
  for (const s of stones) {
    if (s === '0') {
      next.push('1');
    } else if (s.length % 2 === 0) {
      const mid = s.length / 2;
      let left = s.substring(0, mid).replace(/^0+/, '') || '0';
      let right = s.substring(mid).replace(/^0+/, '') || '0';
      next.push(left, right);
    } else {
      next.push((BigInt(s) * 2024n).toString());
    }
  }
  stones = next;
}

console.log(stones.length);
