
import * as fs from 'fs';

function nextSecret(s: number): number {
  let x = s * 64;
  s ^= x;
  s &= 0xFFFFFF;
  x = s / 32;
  s ^= x;
  s &= 0xFFFFFF;
  x = s * 2048;
  s ^= x;
  s &= 0xFFFFFF;
  return s;
}

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n').map(Number);
let total = 0;
const lookup = new Map<number, number>();

for (const b of input) {
  let s = b;
  if (lookup.has(s)) {
    total += lookup.get(s)!;
    continue;
  }
  for (let i = 0; i < 2000; i++) {
    s = nextSecret(s);
  }
  lookup.set(b, s);
  total += s;
}

console.log(total);
