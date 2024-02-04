const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim();
const halfway = data.length / 2;
let sum = 0;

for (let i = 0; i < data.length; i++) {
  const next = (i + halfway) % data.length;
  if (data[i] === data[next]) {
    sum += parseInt(data[i]);
  }
}

console.log(sum);