const fs = require('fs');

const file = fs.readFileSync('input.txt', 'utf8');
const lines = file.split('\n');

const registers = {};

for (const line of lines) {
  const [reg, op, amount, , condReg, condOp, condVal] = line.split(' ');

  const cond = eval(`${registers[condReg] || 0} ${condOp} ${+condVal}`);

  if (cond) {
    registers[reg] = (registers[reg] || 0) + (op === 'inc' ? +amount : -amount);
  }
}

let maxValue = 0;
for (const value of Object.values(registers)) {
  if (value > maxValue) {
    maxValue = value;
  }
}

console.log(maxValue);