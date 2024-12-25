
import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
const grid = input.map((line) => line.trim());
const word = 'XMAS';

const directions = [
  [0, 1],
  [1, 0],
  [1, 1],
  [-1, 1],
  [0, -1],
  [-1, 0],
  [-1, -1],
  [1, -1],
];

let count = 0;
for (let i = 0; i < grid.length; i++) {
  for (let j = 0; j < grid[i].length; j++) {
    if (grid[i][j] === word[0]) {
      for (const [dx, dy] of directions) {
        let x = i;
        let y = j;
        let k = 0;
        while (k < word.length && x >= 0 && y >= 0 && x < grid.length && y < grid[i].length && grid[x][y] === word[k]) {
          x += dx;
          y += dy;
          k++;
        }
        if (k === word.length) {
          count++;
        }
      }
    }
  }
}

console.log(`XMAS appears ${count} times in the word search`);
