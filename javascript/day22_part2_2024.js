
const fs = require('node:fs');

const mod = 1 << 24;
const numSteps = 2000;

const nextSecret = (s) => {
  let x = s * 64n;
  s ^= x;
  s &= BigInt(mod - 1);
  x = s / 32n;
  s ^= x;
  s &= BigInt(mod - 1);
  x = s * 2048n;
  s ^= x;
  s &= BigInt(mod - 1);
  return s;
};

const encodeChange4 = (c1, c2, c3, c4) => {
  return (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19;
};

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  const initials = data.trim().split('\n').map(Number).map(BigInt);

  const buyers = initials.map(initVal => {
    const prices = [];
    let s = initVal;
    for (let j = 0; j <= numSteps; j++) {
      prices.push(Number(s % 10n));
      if (j < numSteps) {
        s = nextSecret(s);
      }
    }
    const changes = [];
    for (let j = 0; j < numSteps; j++) {
      changes.push(prices[j + 1] - prices[j]);
    }
    return { prices, changes };
  });

  const patternCount = 19 * 19 * 19 * 19;
  const globalSum = new Array(patternCount).fill(0n);

  for (const b of buyers) {
    const localPrice = new Array(patternCount).fill(-1);
    for (let i = 0; i + 3 < numSteps; i++) {
      const c1 = b.changes[i], c2 = b.changes[i + 1], c3 = b.changes[i + 2], c4 = b.changes[i + 3];
      if (c1 < -9 || c1 > 9 || c2 < -9 || c2 > 9 ||
        c3 < -9 || c3 > 9 || c4 < -9 || c4 > 9) {
        continue;
      }
      const idx = encodeChange4(c1, c2, c3, c4);
      if (localPrice[idx] < 0) {
        localPrice[idx] = b.prices[i + 4];
      }
    }
    for (let idx = 0; idx < patternCount; idx++) {
      if (localPrice[idx] >= 0) {
        globalSum[idx] += BigInt(localPrice[idx]);
      }
    }
  }

  let ans = 0n;
  for (const s of globalSum) {
    if (s > ans) {
      ans = s;
    }
  }

  console.log(ans.toString());
});
