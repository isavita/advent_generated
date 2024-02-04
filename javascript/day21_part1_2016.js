const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let password = 'abcdefgh';

input.forEach(instruction => {
  const parts = instruction.split(' ');

  if (instruction.startsWith('swap position')) {
    const x = parseInt(parts[2]);
    const y = parseInt(parts[5]);
    const temp = password[x];
    password = password.substr(0, x) + password[y] + password.substr(x + 1);
    password = password.substr(0, y) + temp + password.substr(y + 1);
  } else if (instruction.startsWith('swap letter')) {
    const x = parts[2];
    const y = parts[5];
    password = password.replace(new RegExp(x, 'g'), '_');
    password = password.replace(new RegExp(y, 'g'), x);
    password = password.replace(new RegExp('_', 'g'), y);
  } else if (instruction.startsWith('rotate left')) {
    const steps = parseInt(parts[2]);
    password = password.slice(steps) + password.slice(0, steps);
  } else if (instruction.startsWith('rotate right')) {
    const steps = parseInt(parts[2]);
    password = password.slice(-steps) + password.slice(0, -steps);
  } else if (instruction.startsWith('rotate based')) {
    const x = parts[6];
    const idx = password.indexOf(x);
    let steps = 1 + idx + (idx >= 4 ? 1 : 0);
    steps = steps % password.length;
    password = password.slice(-steps) + password.slice(0, -steps);
  } else if (instruction.startsWith('reverse positions')) {
    const x = parseInt(parts[2]);
    const y = parseInt(parts[4]);
    password = password.substr(0, x) + password.slice(x, y + 1).split('').reverse().join('') + password.substr(y + 1);
  } else if (instruction.startsWith('move position')) {
    const x = parseInt(parts[2]);
    const y = parseInt(parts[5]);
    const letter = password[x];
    password = password.substr(0, x) + password.substr(x + 1);
    password = password.substr(0, y) + letter + password.substr(y);
  }
});

console.log(password);