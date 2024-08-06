const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
let password = 'abcdefgh'; // Change const to let

for (const op of input) {
  password = applyOperation(op, password);
}

console.log(password);

function applyOperation(op, password) {
  const fields = op.split(' ');
  switch (fields[0]) {
    case 'swap':
      switch (fields[1]) {
        case 'position':
          const x = parseInt(fields[2]);
          const y = parseInt(fields[5]);
          return swapPosition(password, x, y);
        case 'letter':
          const x1 = fields[2];
          const y1 = fields[5];
          return swapLetter(password, x1, y1);
      }
    case 'rotate':
      switch (fields[1]) {
        case 'left':
          const steps = parseInt(fields[2]);
          return rotateLeft(password, steps);
        case 'right':
          const steps1 = parseInt(fields[2]);
          return rotateRight(password, steps1);
        case 'based':
          const x2 = fields[6];
          return rotateBasedOnPosition(password, x2);
      }
    case 'reverse':
      const x3 = parseInt(fields[2]);
      const y2 = parseInt(fields[4]);
      return reversePositions(password, x3, y2);
    case 'move':
      const x4 = parseInt(fields[2]);
      const y3 = parseInt(fields[5]);
      return movePosition(password, x4, y3);
  }
}

function swapPosition(password, x, y) {
  const runes = password.split('');
  [runes[x], runes[y]] = [runes[y], runes[x]];
  return runes.join('');
}

function swapLetter(password, x, y) {
  const runes = password.split('');
  for (let i = 0; i < runes.length; i++) {
    if (runes[i] === x) {
      runes[i] = y;
    } else if (runes[i] === y) {
      runes[i] = x;
    }
  }
  return runes.join('');
}

function rotateLeft(password, steps) {
  steps = steps % password.length;
  return password.slice(steps) + password.slice(0, steps);
}

function rotateRight(password, steps) {
  steps = steps % password.length;
  return password.slice(-steps) + password.slice(0, -steps);
}

function rotateBasedOnPosition(password, x) {
  const index = password.indexOf(x);
  let steps = 1 + index;
  if (index >= 4) {
    steps++;
  }
  return rotateRight(password, steps);
}

function reversePositions(password, x, y) {
  const runes = password.split('');
  for (let i = x, j = y; i < j; i++, j--) {
    [runes[i], runes[j]] = [runes[j], runes[i]];
  }
  return runes.join('');
}

function movePosition(password, x, y) {
  const runes = password.split('');
  const r = runes[x];
  runes.splice(x, 1);
  runes.splice(y, 0, r);
  return runes.join('');
}