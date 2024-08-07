import * as fs from 'fs';

type Position = [number, number];
type Keypad = { [key: string]: string };

const keypad1: Keypad = {
  '1': '1', '2': '2', '3': '3',
  '4': '4', '5': '5', '6': '6',
  '7': '7', '8': '8', '9': '9'
};

const keypad2: Keypad = {
  '1': '1', '2': '2', '3': '3', '4': '4',
  '5': '5', '6': '6', '7': '7', '8': '8', '9': '9',
  'A': 'A', 'B': 'B', 'C': 'C', 'D': 'D'
};

const keypad1Layout: (string | null)[][] = [
  ['1', '2', '3'],
  ['4', '5', '6'],
  ['7', '8', '9']
];

const keypad2Layout: (string | null)[][] = [
  [null, null, '1', null, null],
  [null, '2', '3', '4', null],
  ['5', '6', '7', '8', '9'],
  [null, 'A', 'B', 'C', null],
  [null, null, 'D', null, null]
];

function getPosition(keypad: (string | null)[][], value: string): Position {
  for (let y = 0; y < keypad.length; y++) {
    for (let x = 0; x < keypad[y].length; x++) {
      if (keypad[y][x] === value) {
        return [x, y];
      }
    }
  }
  throw new Error(`Value ${value} not found in keypad`);
}

function move(keypad: (string | null)[][], position: Position, direction: string): Position {
  const [x, y] = position;
  switch (direction) {
    case 'U': return keypad[y - 1] && keypad[y - 1][x] ? [x, y - 1] : position;
    case 'D': return keypad[y + 1] && keypad[y + 1][x] ? [x, y + 1] : position;
    case 'L': return keypad[y][x - 1] ? [x - 1, y] : position;
    case 'R': return keypad[y][x + 1] ? [x + 1, y] : position;
    default: return position;
  }
}

function getCode(instructions: string[], keypad: (string | null)[][]): string {
  let position = getPosition(keypad, '5');
  let code = '';

  for (const line of instructions) {
    for (const direction of line) {
      position = move(keypad, position, direction);
    }
    code += keypad[position[1]][position[0]];
  }

  return code;
}

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

const code1 = getCode(input, keypad1Layout);
const code2 = getCode(input, keypad2Layout);

console.log(`Part 1: ${code1}`);
console.log(`Part 2: ${code2}`);