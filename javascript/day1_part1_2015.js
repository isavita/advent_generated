const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();
let floor = 0;

for (let i = 0; i < input.length; i++) {
  if (input[i] === '(') {
    floor++;
  } else if (input[i] === ')') {
    floor--;
  }
}

console.log(floor);