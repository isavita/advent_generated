const fs = require('fs');

const readAll = (path) => {
  return fs.readFileSync(path, 'utf8');
};

const input = readAll('input.txt').split('\n');

let x = [1];
input.forEach((line) => {
  switch (line) {
    case 'noop':
      x.push(x[x.length - 1]);
      break;
    default:
      const n = parseInt(line.split(' ')[1]);
      x.push(x[x.length - 1]);
      x.push(x[x.length - 1] + n);
  }
});

let sum = 0;
x.forEach((value, index) => {
  if ((index - 19) % 40 === 0) {
    sum += (index + 1) * value;
  }
});

console.log(sum);