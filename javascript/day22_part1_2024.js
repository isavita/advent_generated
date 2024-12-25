
const fs = require('fs');

function nextSecret(s) {
  let x = s * 64;
  s ^= x;
  s &= 0xFFFFFF;
  x = Math.floor(s / 32);
  s ^= x;
  s &= 0xFFFFFF;
  x = s * 2048;
  s ^= x;
  s &= 0xFFFFFF;
  return s;
}

try {
  const data = fs.readFileSync('input.txt', 'utf8');
  const buyers = data.trim().split('\n').map(Number);

  let total = 0;
  for (const b of buyers) {
    let s = b;
    for (let i = 0; i < 2000; i++) {
      s = nextSecret(s);
    }
    total += s;
  }
  console.log(total);
} catch (err) {
  console.error(err);
}
