
const fs = require('fs');
const readline = require('readline');

const file = fs.createReadStream('input.txt');
const rl = readline.createInterface({
  input: file,
  output: process.stdout,
  terminal: false
});

const instructions = [];
let registers = {a: 0, b: 0, c: 1, d: 0};

rl.on('line', (line) => {
  instructions.push(line);
});

rl.on('close', () => {
  executeInstructions(instructions, registers);
  console.log(registers["a"]);
});

function executeInstructions(instructions, registers) {
  for (let i = 0; i < instructions.length;) {
    const parts = instructions[i].split(' ');
    switch (parts[0]) {
      case "cpy":
        const copyVal = getValue(parts[1], registers);
        registers[parts[2]] = copyVal;
        i++;
        break;
      case "inc":
        registers[parts[1]]++;
        i++;
        break;
      case "dec":
        registers[parts[1]]--;
        i++;
        break;
      case "jnz":
        const jumpVal = getValue(parts[1], registers);
        if (jumpVal !== 0) {
          const jump = parseInt(parts[2]);
          i += jump;
        } else {
          i++;
        }
        break;
    }
  }
}

function getValue(s, registers) {
  const val = parseInt(s);
  if (isNaN(val)) {
    return registers[s];
  }
  return val;
}
