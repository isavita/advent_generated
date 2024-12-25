
const fs = require('fs');

try {
  const data = fs.readFileSync('input.txt', 'utf8');
  const raw = data.split('\n').map(line => line.trim()).filter(line => line !== '');

  if (raw.length % 7 !== 0) {
    console.log(0);
    return;
  }

  const locks = [];
  const keys = [];

  for (let i = 0; i + 7 <= raw.length; i += 7) {
    const block = raw.slice(i, i + 7);
    if (block.some(ln => ln.length < 5)) continue;

    if (block[0].split('').every(char => char === '#')) {
      locks.push(parseLock(block));
    } else {
      keys.push(parseKey(block));
    }
  }

  let count = 0;
  for (const lock of locks) {
    for (const key of keys) {
      if (fits(lock, key)) {
        count++;
      }
    }
  }
  console.log(count);

} catch (err) {
  console.error(err);
}

function parseLock(b) {
  const h = Array(5).fill(0);
  for (let c = 0; c < 5; c++) {
    for (let r = 1; r < 7; r++) {
      if (b[r][c] === '#') {
        h[c]++;
      } else {
        break;
      }
    }
  }
  return h;
}

function parseKey(b) {
  const h = Array(5).fill(0);
  for (let c = 0; c < 5; c++) {
    for (let r = 5; r >= 0; r--) {
      if (b[r][c] === '#') {
        h[c]++;
      } else {
        break;
      }
    }
  }
  return h;
}

function fits(lock, key) {
  for (let i = 0; i < 5; i++) {
    if (lock[i] + key[i] > 5) {
      return false;
    }
  }
  return true;
}
