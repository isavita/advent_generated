
const fs = require('fs');

const evenDigits = (s) => s.length % 2 === 0;

const trimLeadingZeros = (s) => {
  while (s.length > 1 && s[0] === '0') {
    s = s.substring(1);
  }
  return s;
};

const solve = () => {
  let stones = fs.readFileSync('input.txt', 'utf-8').trim().split(/\s+/);

  for (let i = 0; i < 25; i++) {
    const next = [];
    for (const s of stones) {
      if (s === '0') {
        next.push('1');
      } else if (evenDigits(s)) {
        const mid = s.length / 2;
        let left = trimLeadingZeros(s.substring(0, mid));
        let right = trimLeadingZeros(s.substring(mid));
        if (left === '') left = '0';
        if (right === '') right = '0';
        next.push(left, right);
      } else {
        next.push(String(parseInt(s) * 2024));
      }
    }
    stones = next;
  }
  console.log(stones.length);
};

solve();
