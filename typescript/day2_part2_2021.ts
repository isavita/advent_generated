
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

let horizontalPosition = 0;
let depth = 0;
let aim = 0;

input.forEach((line) => {
  const command = line.split(' ');
  const direction = command[0];
  const units = parseInt(command[1]);

  switch (direction) {
    case 'forward':
      horizontalPosition += units;
      depth += aim * units;
      break;
    case 'down':
      aim += units;
      break;
    case 'up':
      aim -= units;
      break;
  }
});

const product = horizontalPosition * depth;
console.log(product);
