
import * as fs from 'fs';

const solve = () => {
  const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n').filter(line => line.trim() !== '');
  if (input.length % 7 !== 0) {
    console.log(0);
    return;
  }

  const locks: number[][] = [];
  const keys: number[][] = [];

  for (let i = 0; i + 7 <= input.length; i += 7) {
    const block = input.slice(i, i + 7);
    if (block.some(line => line.length < 5)) continue;

    const isLock = block[0].split('').every(char => char === '#');
    const parsed = parse(block);
    if (isLock) locks.push(parsed);
    else keys.push(parsed);
  }

  let count = 0;
  for (const lock of locks) {
    for (const key of keys) {
      if (fits(lock, key)) count++;
    }
  }
  console.log(count);
};

const parse = (block: string[]): number[] => {
  const h = Array(5).fill(0);
  const isLock = block[0].split('').every(char => char === '#');

  for (let c = 0; c < 5; c++) {
    for (let r = isLock ? 1 : 5; isLock ? r < 7 : r >= 0; isLock ? r++ : r--) {
      if (block[r][c] === '#') h[c]++;
      else break;
    }
  }
  return h;
};

const fits = (lock: number[], key: number[]): boolean => {
  for (let i = 0; i < 5; i++) {
    if (lock[i] + key[i] > 5) return false;
  }
  return true;
};

solve();
