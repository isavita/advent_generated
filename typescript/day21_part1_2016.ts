import * as fs from 'fs';

function main(): void {
  const input = fs.readFileSync('input.txt', 'utf8');
  const operations = input.trim().split('\n');
  let password = 'abcdefgh';

  for (const op of operations) {
    password = applyOperation(op, password);
  }

  console.log(password);
}

function applyOperation(op: string, password: string): string {
  const fields = op.split(' ');
  switch (fields[0]) {
    case 'swap':
      if (fields[1] === 'position') {
        const x = parseInt(fields[2]);
        const y = parseInt(fields[5]);
        return swapPosition(password, x, y);
      } else {
        const letterX = fields[2];
        const letterY = fields[5];
        return swapLetter(password, letterX, letterY);
      }
    case 'rotate':
      if (fields[1] === 'left') {
        const steps = parseInt(fields[2]);
        return rotateLeft(password, steps);
      } else if (fields[1] === 'right') {
        const steps = parseInt(fields[2]);
        return rotateRight(password, steps);
      } else {
        const letterX = fields[6];
        return rotateBasedOnPosition(password, letterX);
      }
    case 'reverse':
      const startX = parseInt(fields[2]);
      const endY = parseInt(fields[4]);
      return reversePositions(password, startX, endY);
    case 'move':
      const moveX = parseInt(fields[2]);
      const moveY = parseInt(fields[5]);
      return movePosition(password, moveX, moveY);
    default:
      throw new Error('Unsupported operation');
  }
}

function swapPosition(password: string, x: number, y: number): string {
  const runes = Array.from(password);
  [runes[x], runes[y]] = [runes[y], runes[x]];
  return runes.join('');
}

function swapLetter(password: string, x: string, y: string): string {
  const runes = Array.from(password);
  for (let i = 0; i < runes.length; i++) {
    if (runes[i] === x) {
      runes[i] = y;
    } else if (runes[i] === y) {
      runes[i] = x;
    }
  }
  return runes.join('');
}

function rotateLeft(password: string, steps: number): string {
  steps = steps % password.length;
  return password.slice(steps) + password.slice(0, steps);
}

function rotateRight(password: string, steps: number): string {
  steps = steps % password.length;
  return password.slice(-steps) + password.slice(0, -steps);
}

function rotateBasedOnPosition(password: string, x: string): string {
  const index = password.indexOf(x);
  let steps = 1 + index;
  if (index >= 4) {
    steps++;
  }
  return rotateRight(password, steps);
}

function reversePositions(password: string, x: number, y: number): string {
  const runes = Array.from(password);
  for (let i = x, j = y; i < j; i++, j--) {
    [runes[i], runes[j]] = [runes[j], runes[i]];
  }
  return runes.join('');
}

function movePosition(password: string, x: number, y: number): string {
  const runes = Array.from(password);
  const r = runes.splice(x, 1)[0];
  runes.splice(y, 0, r);
  return runes.join('');
}

main();