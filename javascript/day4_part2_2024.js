
const fs = require('node:fs');

function checkMAS(grid, x, y, dx, dy) {
  const word = "MAS";
  const len = word.length;
  let forward = true;
  let backward = true;

  for (let i = 0; i < len; i++) {
    const newX = x + dx * i;
    const newY = y + dy * i;
    if (newX < 0 || newY < 0 || newX >= grid.length || newY >= grid[0].length || grid[newX][newY] !== word[i]) {
      forward = false;
    }
    if (newX < 0 || newY < 0 || newX >= grid.length || newY >= grid[0].length || grid[newX][newY] !== word[len - 1 - i]) {
      backward = false;
    }
  }
  return forward || backward;
}

function checkXMAS(grid, x, y) {
  return (checkMAS(grid, x - 1, y - 1, 1, 1) && checkMAS(grid, x - 1, y + 1, 1, -1)) ||
         (checkMAS(grid, x + 1, y - 1, -1, 1) && checkMAS(grid, x + 1, y + 1, -1, -1));
}

function countXMASPatterns(grid) {
  let count = 0;
  for (let i = 1; i < grid.length - 1; i++) {
    for (let j = 1; j < grid[i].length - 1; j++) {
      if (grid[i][j] === 'A' && checkXMAS(grid, i, j)) {
        count++;
      }
    }
  }
  return count;
}

try {
  const data = fs.readFileSync('input.txt', 'utf8');
  const grid = data.trim().split('\n').map(line => line.trim());
  const count = countXMASPatterns(grid);
  console.log(`X-MAS patterns appear ${count} times in the word search`);
} catch (err) {
  console.error("Error reading or processing file:", err);
}

