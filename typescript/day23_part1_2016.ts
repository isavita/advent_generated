const fs = require('fs');

function main() {
  const instructions = readInstructions('input.txt');
  const registers = { a: 7, b: 0, c: 0, d: 0 };
  executeInstructions(instructions, registers);
  console.log(registers.a);
}

function readInstructions(filename) {
  return fs.readFileSync(filename, 'utf8').split('\n');
}

function executeInstructions(instructions, registers) {
  let pc = 0;
  while (pc < instructions.length) {
    const fields = instructions[pc].split(' ');
    switch (fields[0]) {
      case 'cpy':
        const x = getValue(fields[1], registers);
        if (fields[2] in registers) {
          registers[fields[2]] = x;
        }
        break;
      case 'inc':
        if (fields[1] in registers) {
          registers[fields[1]]++;
        }
        break;
      case 'dec':
        if (fields[1] in registers) {
          registers[fields[1]]--;
        }
        break;
      case 'jnz':
        const jnzValue = getValue(fields[1], registers);
        if (jnzValue !== 0) {
          pc += getValue(fields[2], registers) - 1;
        }
        break;
      case 'tgl':
        const toggleValue = getValue(fields[1], registers);
        const tgt = pc + toggleValue;
        if (tgt >= 0 && tgt < instructions.length) {
          instructions[tgt] = toggleInstruction(instructions[tgt]);
        }
        break;
    }
    pc++;
  }
}

function getValue(s, registers) {
  const val = parseInt(s);
  return isNaN(val) ? registers[s] : val;
}

function toggleInstruction(instr) {
  const parts = instr.split(' ');
  switch (parts[0]) {
    case 'inc':
      parts[0] = 'dec';
      break;
    case 'dec':
    case 'tgl':
      parts[0] = 'inc';
      break;
    case 'jnz':
      parts[0] = 'cpy';
      break;
    case 'cpy':
      parts[0] = 'jnz';
      break;
  }
  return parts.join(' ');
}

main();