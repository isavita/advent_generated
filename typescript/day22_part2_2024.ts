
const fs = require('fs');

const MOD = 1 << 24;
const NUM_STEPS = 2000;

function nextSecret(s: number): number {
  let x = s * 64;
  s ^= x;
  s &= MOD - 1;
  x = Math.floor(s / 32);
  s ^= x;
  s &= MOD - 1;
  x = s * 2048;
  s ^= x;
  s &= MOD - 1;
  return s;
}

function encodeChange4(c1: number, c2: number, c3: number, c4: number): number {
  return (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19;
}

const input: string = fs.readFileSync('input.txt', 'utf-8');
const initials: number[] = input.trim().split('\n').filter(line => line !== '').map(Number);

const buyers = initials.map(initVal => {
  const prices: number[] = [];
  let s = initVal;
  for (let j = 0; j <= NUM_STEPS; j++) {
    prices[j] = s % 10;
    if (j < NUM_STEPS) {
      s = nextSecret(s);
    }
  }
  const changes: number[] = [];
  for (let j = 0; j < NUM_STEPS; j++) {
    changes[j] = prices[j + 1] - prices[j];
  }
  return { prices, changes };
});

const PATTERN_COUNT = 19 * 19 * 19 * 19;
const globalSum = new Uint32Array(PATTERN_COUNT);

for (const b of buyers) {
  const localPrice = new Int32Array(PATTERN_COUNT).fill(-1);
  for (let i = 0; i + 3 < NUM_STEPS; i++) {
    const c1 = b.changes[i];
    const c2 = b.changes[i + 1];
    const c3 = b.changes[i + 2];
    const c4 = b.changes[i + 3];
    if (c1 < -9 || c1 > 9 || c2 < -9 || c2 > 9 || c3 < -9 || c3 > 9 || c4 < -9 || c4 > 9) {
      continue;
    }
    const idx = encodeChange4(c1, c2, c3, c4);
    if (localPrice[idx] < 0) {
      localPrice[idx] = b.prices[i + 4];
    }
  }
  for (let idx = 0; idx < PATTERN_COUNT; idx++) {
    if (localPrice[idx] >= 0) {
      globalSum[idx] += localPrice[idx];
    }
  }
}

let ans = 0;
for (let i = 0; i < PATTERN_COUNT; i++) {
  if (globalSum[i] > ans) {
    ans = globalSum[i];
  }
}

console.log(ans);
