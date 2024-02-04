const fs = require('fs');

function fromSnafu(s) {
  let n = 0;
  for (let i = 0; i < s.length; i++) {
    n *= 5;
    switch (s[i]) {
      case '=':
        n -= 2;
        break;
      case '-':
        n--;
        break;
      default:
        n += parseInt(s[i]);
    }
  }
  return n;
}

function toSnafu(n) {
  let b = [];
  while (n > 0) {
    switch (n % 5) {
      case 3:
        n += 5;
        b.push('=');
        break;
      case 4:
        n += 5;
        b.push('-');
        break;
      default:
        b.push(String(n % 5));
    }
    n = Math.floor(n / 5);
  }
  return b.reverse().join('');
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  const lines = data.split('\n');
  let sum = 0;
  lines.forEach(line => {
    sum += fromSnafu(line);
  });
  console.log(toSnafu(sum));
});