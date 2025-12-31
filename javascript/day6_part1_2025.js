
const fs = require('fs');

function main() {
  const data = fs.readFileSync('input.txt', 'utf8');
  const lines = data.split(/\r?\n/);
  if (lines.length === 1 && lines[0] === '') {
    console.log('Grand total: 0');
    return;
  }

  const maxWidth = lines.reduce((m, l) => Math.max(m, l.length), 0);
  const isSep = new Array(maxWidth).fill(true);

  for (let x = 0; x < maxWidth; x++) {
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i];
      if (x < line.length && !/\s/.test(line[x])) {
        isSep[x] = false;
        break;
      }
    }
  }

  let total = 0n;
  let inBlock = false;
  let start = 0;

  for (let x = 0; x < maxWidth; x++) {
    if (!isSep[x]) {
      if (!inBlock) {
        inBlock = true;
        start = x;
      }
    } else {
      if (inBlock) {
        total += processBlock(lines, start, x - 1);
        inBlock = false;
      }
    }
  }
  if (inBlock) total += processBlock(lines, start, maxWidth - 1);

  console.log(`Grand total: ${total}`);
}

function processBlock(lines, start, end) {
  let op = '';
  const nums = [];

  for (const line of lines) {
    if (start < line.length) {
      const seg = line.slice(start, Math.min(end + 1, line.length)).trim();
      if (seg) {
        if (seg === '+' || seg === '*') {
          op = seg;
        } else {
          try {
            nums.push(BigInt(seg));
          } catch (_) {}
        }
      }
    }
  }

  if (nums.length === 0) return 0n;
  if (op === '+') return nums.reduce((a, b) => a + b, 0n);
  if (op === '*') return nums.reduce((a, b) => a * b, 1n);
  if (nums.length === 1) return nums[0];
  return 0n;
}

main();
