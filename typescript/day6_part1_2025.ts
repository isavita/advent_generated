
import * as fs from 'fs';

const lines = fs.readFileSync('input.txt', 'utf8').split(/\r?\n/);
let maxw = 0;
for (const l of lines) if (l.length > maxw) maxw = l.length;

function isSep(col: number): boolean {
  for (const line of lines) {
    if (col < line.length && !/\s/.test(line[col])) return false;
  }
  return true;
}

function processBlock(sc: number, ec: number, grand: bigint): bigint {
  const nums: bigint[] = [];
  let op: '+' | '*' | null = null;
  for (const line of lines) {
    const seg = line.slice(sc, ec + 1).trim();
    if (!seg) continue;
    if (seg === '+') op = '+';
    else if (seg === '*') op = '*';
    else nums.push(BigInt(seg));
  }
  if (nums.length === 0) return grand;
  let acc: bigint;
  if (op === '+') acc = nums.reduce((a, b) => a + b, 0n);
  else if (op === '*') acc = nums.reduce((a, b) => a * b, 1n);
  else acc = nums[0];
  return grand + acc;
}

let grand = 0n;
let inb = false, sc = 0;
for (let x = 0; x < maxw; x++) {
  if (!isSep(x)) {
    if (!inb) { inb = true; sc = x; }
  } else {
    if (inb) { grand = processBlock(sc, x - 1, grand); inb = false; }
  }
}
if (inb) grand = processBlock(sc, maxw - 1, grand);

console.log(`Grand total: ${grand.toString()}`);
