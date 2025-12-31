
const fs = require('fs');

function maxSubseq(s, k) {
  const n = s.length;
  let toRemove = n - k;
  const stack = new Array(n);
  let top = 0;

  for (let i = 0; i < n; i++) {
    const d = s[i];
    while (toRemove > 0 && top > 0 && stack[top - 1] < d) {
      top--;
      toRemove--;
    }
    stack[top++] = d;
  }
  return stack.slice(0, k).join('');
}

function main() {
  const data = fs.readFileSync('input.txt', 'utf8');
  const lines = data.split(/\r?\n/);
  const k = 12;
  let total = 0n;

  for (const raw of lines) {
    const s = raw.trim();
    if (s && s.length >= k) {
      const best = maxSubseq(s, k);
      total += BigInt(best);
    }
  }

  console.log(`Total output joltage: ${total}`);
}

main();
