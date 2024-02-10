const fs = require('fs');
const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

class Position {
  constructor(x, y, dirIndex) {
    this.x = x;
    this.y = y;
    this.dirIndex = dirIndex;
  }
}

const instructions = input[0].split(', ');

const pos = new Position(0, 0, 0);
const directions = [[0, 1], [1, 0], [0, -1], [-1, 0]];

for (let instruction of instructions) {
  const turn = instruction[0];
  const blocks = parseInt(instruction.substring(1));

  if (turn === 'R') {
    pos.dirIndex = (pos.dirIndex + 1) % 4;
  } else {
    pos.dirIndex = (pos.dirIndex - 1 + 4) % 4;
  }

  pos.x += directions[pos.dirIndex][0] * blocks;
  pos.y += directions[pos.dirIndex][1] * blocks;
}

console.log(Math.abs(pos.x) + Math.abs(pos.y));