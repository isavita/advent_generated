import * as fs from 'fs';

// Define the dimensions of the screen
const SCREEN_WIDTH = 50;
const SCREEN_HEIGHT = 6;

// Initialize the screen as a 2D array of boolean values (false means off, true means on)
let screen: boolean[][] = Array.from({ length: SCREEN_HEIGHT }, () =>
  Array(SCREEN_WIDTH).fill(false)
);

// Function to turn on a rectangle of pixels
function rect(A: number, B: number) {
  for (let y = 0; y < B; y++) {
    for (let x = 0; x < A; x++) {
      screen[y][x] = true;
    }
  }
}

// Function to rotate a row of pixels
function rotateRow(A: number, B: number) {
  const row = screen[A];
  for (let i = 0; i < B; i++) {
    row.unshift(row.pop()!);
  }
}

// Function to rotate a column of pixels
function rotateColumn(A: number, B: number) {
  const column: boolean[] = [];
  for (let y = 0; y < SCREEN_HEIGHT; y++) {
    column.push(screen[y][A]);
  }
  for (let i = 0; i < B; i++) {
    column.unshift(column.pop()!);
  }
  for (let y = 0; y < SCREEN_HEIGHT; y++) {
    screen[y][A] = column[y];
  }
}

// Function to process a single instruction
function processInstruction(instruction: string) {
  if (instruction.startsWith('rect')) {
    const [A, B] = instruction.split(' ')[1].split('x').map(Number);
    rect(A, B);
  } else if (instruction.startsWith('rotate row')) {
    const [, A, B] = instruction.match(/y=(\d+) by (\d+)/)!.map(Number);
    rotateRow(A, B);
  } else if (instruction.startsWith('rotate column')) {
    const [, A, B] = instruction.match(/x=(\d+) by (\d+)/)!.map(Number);
    rotateColumn(A, B);
  }
}

// Read the input file and process each instruction
const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
input.forEach(processInstruction);

// Count the number of lit pixels
const litPixels = screen.flat().filter(pixel => pixel).length;

// Print the result
console.log(litPixels);