const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let registers = { a: 0, b: 0, c: 1, d: 0 };
let i = 0;

while (i < data.length) {
  const [op, x, y] = data[i].split(' ');

  switch (op) {
    case 'cpy':
      registers[y] = isNaN(parseInt(x)) ? registers[x] : parseInt(x);
      break;
    case 'inc':
      registers[x]++;
      break;
    case 'dec':
      registers[x]--;
      break;
    case 'jnz':
      if ((isNaN(parseInt(x)) ? registers[x] : parseInt(x)) !== 0) {
        i += parseInt(y) - 1;
      }
      break;
  }

  i++;
}

console.log(registers.a);