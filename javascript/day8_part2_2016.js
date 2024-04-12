const fs = require('fs');

const screenWidth = 50;
const screenHeight = 6;

const screen = Array(screenHeight).fill().map(() => Array(screenWidth).fill(false));

const input = fs.readFileSync('input.txt', 'utf-8');
const instructions = input.split('\n');

instructions.forEach(instruction => {
  processInstruction(instruction, screen);
});

displayScreen(screen);

function displayScreen(screen) {
  screen.forEach(row => {
    console.log(row.map(pixel => pixel ? '#' : '.').join(''));
  });
}

function processInstruction(instruction, screen) {
  const rectRegex = /rect (\d+)x(\d+)/;
  const rotateRowRegex = /rotate row y=(\d+) by (\d+)/;
  const rotateColumnRegex = /rotate column x=(\d+) by (\d+)/;

  if (rectRegex.test(instruction)) {
    const [, a, b] = instruction.match(rectRegex);
    rect(screen, parseInt(a), parseInt(b));
  } else if (rotateRowRegex.test(instruction)) {
    const [, a, b] = instruction.match(rotateRowRegex);
    rotateRow(screen, parseInt(a), parseInt(b));
  } else if (rotateColumnRegex.test(instruction)) {
    const [, a, b] = instruction.match(rotateColumnRegex);
    rotateColumn(screen, parseInt(a), parseInt(b));
  }
}

function rect(screen, a, b) {
  for (let y = 0; y < b; y++) {
    for (let x = 0; x < a; x++) {
      screen[y][x] = true;
    }
  }
}

function rotateRow(screen, row, shift) {
  const temp = Array(screenWidth);
  for (let i = 0; i < screenWidth; i++) {
    temp[(i + shift) % screenWidth] = screen[row][i];
  }
  screen[row] = temp;
}

function rotateColumn(screen, col, shift) {
  const temp = Array(screenHeight);
  for (let i = 0; i < screenHeight; i++) {
    temp[(i + shift) % screenHeight] = screen[i][col];
  }
  for (let i = 0; i < screenHeight; i++) {
    screen[i][col] = temp[i];
  }
}