const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

const registers = {};
let highestValue = 0;

input.forEach(line => {
  const parts = line.split(' ');
  const reg = parts[0];
  const op = parts[1];
  const amount = parseInt(parts[2]);
  const condReg = parts[4];
  const condOp = parts[5];
  const condVal = parseInt(parts[6]);

  let cond = false;
  switch (condOp) {
    case '>':
      cond = (registers[condReg] || 0) > condVal;
      break;
    case '>=':
      cond = (registers[condReg] || 0) >= condVal;
      break;
    case '<':
      cond = (registers[condReg] || 0) < condVal;
      break;
    case '<=':
      cond = (registers[condReg] || 0) <= condVal;
      break;
    case '==':
      cond = (registers[condReg] || 0) == condVal;
      break;
    case '!=':
      cond = (registers[condReg] || 0) != condVal;
      break;
  }

  if (cond) {
    switch (op) {
      case 'inc':
        registers[reg] = (registers[reg] || 0) + amount;
        break;
      case 'dec':
        registers[reg] = (registers[reg] || 0) - amount;
        break;
    }

    if ((registers[reg] || 0) > highestValue) {
      highestValue = registers[reg];
    }
  }
});

console.log(highestValue);