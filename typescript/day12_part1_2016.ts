const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const registers = { a: 0, b: 0, c: 0, d: 0 };

executeInstructions(input, registers);

console.log(registers.a);

function executeInstructions(instructions, registers) {
  let i = 0;
  while (i < instructions.length) {
    const parts = instructions[i].trim().split(' ');
    switch (parts[0]) {
      case 'cpy':
        const value = getValue(parts[1], registers);
        registers[parts[2]] = value;
        i++;
        break;
      case 'inc':
        registers[parts[1]]++;
        i++;
        break;
      case 'dec':
        registers[parts[1]]--;
        i++;
        break;
      case 'jnz':
        const jumpValue = getValue(parts[1], registers);
        if (jumpValue !== 0) {
          const jump = parseInt(parts[2], 10);
          i += jump;
        } else {
          i++;
        }
        break;
    }
  }
}

function getValue(s, registers) {
  const parsedValue = parseInt(s, 10);
  return isNaN(parsedValue) ? registers[s] : parsedValue;
}