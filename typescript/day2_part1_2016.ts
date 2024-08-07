import * as fs from 'fs';

// Define the keypad layout
const keypad: number[][] = [
  [1, 2, 3],
  [4, 5, 6],
  [7, 8, 9]
];

// Define the movements
const moves = {
  U: [-1, 0],
  D: [1, 0],
  L: [0, -1],
  R: [0, 1]
};

// Function to get the new position after a move
function getNewPosition(current: [number, number], move: string): [number, number] {
  const [dx, dy] = moves[move as keyof typeof moves];
  const [x, y] = current;
  const newX = x + dx;
  const newY = y + dy;

  if (newX >= 0 && newX < keypad.length && newY >= 0 && newY < keypad[newX].length) {
    return [newX, newY];
  }

  return current;
}

// Function to get the bathroom code
function getBathroomCode(instructions: string[]): string {
  let position: [number, number] = [1, 1]; // Start at '5'
  let code = '';

  for (const line of instructions) {
    for (const move of line) {
      position = getNewPosition(position, move);
    }
    code += keypad[position[0]][position[1]];
  }

  return code;
}

// Read input from file
const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

// Get the bathroom code
const bathroomCode = getBathroomCode(input);

// Print the output
console.log(bathroomCode);