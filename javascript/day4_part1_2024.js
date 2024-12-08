
const fs = require('node:fs');

function countXMAS(grid) {
  const rows = grid.length;
  const cols = grid[0].length;
  const target = "XMAS";
  let count = 0;

  // Helper function to check a sequence
  const checkSequence = (r, c, dr, dc) => {
    for (let i = 0; i < target.length; i++) {
      if (r + i * dr < 0 || r + i * dr >= rows || c + i * dc < 0 || c + i * dc >= cols || grid[r + i * dr][c + i * dc] !== target[i]) {
        return false;
      }
    }
    return true;
  };

  // Check all directions for each cell
  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      //Horizontal
      if (checkSequence(r, c, 0, 1)) count++;
      if (checkSequence(r, c, 0, -1)) count++;
      //Vertical
      if (checkSequence(r, c, 1, 0)) count++;
      if (checkSequence(r, c, -1, 0)) count++;
      //Diagonal
      if (checkSequence(r, c, 1, 1)) count++;
      if (checkSequence(r, c, 1, -1)) count++;
      if (checkSequence(r, c, -1, 1)) count++;
      if (checkSequence(r, c, -1, -1)) count++;
    }
  }
  return count;
}


fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error("Failed to read the file:", err);
    return;
  }

  const grid = data.trim().split('\n').map(line => line.split(''));
  const xmasCount = countXMAS(grid);
  console.log("Number of XMAS occurrences:", xmasCount);
});

