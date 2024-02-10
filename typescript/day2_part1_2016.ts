
const fs = require('fs');

const file = fs.readFileSync('input.txt', 'utf8');
const instructions = file.split('\n');

const keypad = [
  [1, 2, 3],
  [4, 5, 6],
  [7, 8, 9]
];
let x = 1, y = 1;
let code = '';

instructions.forEach(instruction => {
  instruction.split('').forEach(move => {
    switch (move) {
      case 'U':
        if (x > 0) x--;
        break;
      case 'D':
        if (x < 2) x++;
        break;
      case 'L':
        if (y > 0) y--;
        break;
      case 'R':
        if (y < 2) y++;
        break;
    }
  });
  code += keypad[x][y];
});

console.log(code);
