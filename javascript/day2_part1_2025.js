
const fs = require('fs');

function main() {
  const raw = fs.readFileSync('input.txt', 'utf8').replace(/[\r\n]/g, '').trim();
  if (!raw) return console.log(0);
  const ranges = raw.split(',').filter(Boolean).map(p => {
    const [s, e] = p.split('-');
    return { start: BigInt(s), end: BigInt(e) };
  });

  const maxEnd = ranges.reduce((m, r) => r.end > m ? r.end : m, 0n);
  const maxK = Math.ceil(maxEnd.toString().length / 2);

  const ids = new Set();

  for (let k = 1; k <= maxK; k++) {
    const pow10k = 10n ** BigInt(k);
    const mul = pow10k + 1n;
    const minSeed = k === 1 ? 1n : 10n ** BigInt(k - 1);
    const maxSeed = pow10k - 1n;

    for (const r of ranges) {
      const sMin = (r.start + mul - 1n) / mul;
      const sMax = r.end / mul;
      const low = sMin > minSeed ? sMin : minSeed;
      const high = sMax < maxSeed ? sMax : maxSeed;
      for (let cur = low; cur <= high; cur++) {
        ids.add(cur * mul);
      }
    }
  }

  let total = 0n;
  for (const v of ids) total += v;
  console.log(total.toString());
}

main();
