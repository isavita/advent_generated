const fs = require('fs');

const gridSize = 300;
const grid = Array(gridSize).fill().map(() => Array(gridSize).fill(0));

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  const serial = parseInt(data.trim(), 10);

  for (let y = 0; y < gridSize; y++) {
    for (let x = 0; x < gridSize; x++) {
      const rackID = x + 11;
      let powerLevel = rackID * (y + 1);
      powerLevel += serial;
      powerLevel *= rackID;
      powerLevel = Math.floor(powerLevel / 100) % 10;
      powerLevel -= 5;
      grid[y][x] = powerLevel;
    }
  }

  let maxPower = -Infinity;
  let maxX = 0;
  let maxY = 0;
  let maxSize = 0;

  for (let size = 1; size <= gridSize; size++) {
    for (let y = 0; y < gridSize - size + 1; y++) {
      for (let x = 0; x < gridSize - size + 1; x++) {
        let totalPower = 0;
        for (let dy = 0; dy < size; dy++) {
          for (let dx = 0; dx < size; dx++) {
            totalPower += grid[y + dy][x + dx];
          }
        }
        if (totalPower > maxPower) {
          maxPower = totalPower;
          maxX = x + 1;
          maxY = y + 1;
          maxSize = size;
        }
      }
    }
  }

  console.log(`${maxX},${maxY},${maxSize}`);
});