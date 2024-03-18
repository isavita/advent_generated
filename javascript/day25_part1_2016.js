const fs = require('fs');

function producesClockSignal(a, instructions) {
  const registers = { a, b: 0, c: 0, d: 0 };
  let lastOutput = 0, outputCount = 0;

  for (let i = 0; i < instructions.length; i++) {
    const parts = instructions[i].split(' ');
    switch (parts[0]) {
      case 'cpy':
        const val = getValue(parts[1], registers);
        registers[parts[2]] = val;
        break;
      case 'inc':
        registers[parts[1]]++;
        break;
      case 'dec':
        registers[parts[1]]--;
        break;
      case 'jnz':
        const jmpVal = getValue(parts[1], registers);
        if (jmpVal !== 0) {
          const jump = parseInt(parts[2]);
          i += jump - 1;
        }
        break;
      case 'out':
        const outVal = getValue(parts[1], registers);
        if (outVal !== 0 && outVal !== 1) {
          return false;
        }
        if (outputCount > 0 && outVal === lastOutput) {
          return false;
        }
        lastOutput = outVal;
        outputCount++;
        if (outputCount > 50) {
          return true;
        }
        break;
    }
  }
  return false;
}

function getValue(s, registers) {
  const num = parseInt(s);
  if (isNaN(num)) {
    return registers[s];
  }
  return num;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  const instructions = data.trim().split('\n');
  for (let a = 1; ; a++) {
    if (producesClockSignal(a, instructions)) {
      console.log(a);
      break;
    }
  }
});