
const fs = require('fs');

(() => {
  const lines = fs.readFileSync('input.txt', 'utf8').split('\n');
  let pos = 50, zeros = 0;
  for (let raw of lines) {
    const line = raw.trim();
    if (!line) continue;
    const dir = line[0];
    const amount = +line.slice(1);
    pos = (dir === 'R' ? pos + amount : pos - amount) % 100;
    if (pos < 0) pos += 100;
    if (pos === 0) ++zeros;
  }
  console.log(`The password is: ${zeros}`);
})();
