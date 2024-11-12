const fs = require('fs');
const pathModule = require('path'); // Renamed to avoid conflict

// Directions: Right=0, Down=1, Left=2, Up=3
const DIRECTIONS = {
  0: { dx: 1, dy: 0 },
  1: { dx: 0, dy: 1 },
  2: { dx: -1, dy: 0 },
  3: { dx: 0, dy: -1 },
};

// Read and parse the input file
function readInput(filePath) {
  const data = fs.readFileSync(pathModule.resolve(__dirname, filePath), 'utf-8');
  const [mapPart, pathPart] = data.split('\n\n');
  const map = mapPart.split('\n').map(line => line.split(''));
  const instructions = parsePath(pathPart.trim());
  return { map, instructions };
}

// Parse the movement path into an array of instructions
function parsePath(pathStr) {
  const regex = /(\d+|[LR])/g;
  return pathStr.match(regex);
}

// Find the starting position: leftmost open tile in the top row
function findStart(map) {
  const firstRow = map[0];
  const x = firstRow.findIndex(cell => cell === '.');
  return { x, y: 0 };
}

// Get the boundaries for each row and column to handle wrapping
function getBoundaries(map) {
  const rowBounds = map.map(row => {
    let first = row.findIndex(cell => cell !== ' ');
    let last = row.length - 1;
    while (last >= 0 && row[last] === ' ') last--;
    return { first, last };
  });

  const colBounds = [];
  const numCols = Math.max(...map.map(row => row.length));
  for (let x = 0; x < numCols; x++) {
    let first = -1;
    let last = -1;
    for (let y = 0; y < map.length; y++) {
      if (x < map[y].length && map[y][x] !== ' ') {
        if (first === -1) first = y;
        last = y;
      }
    }
    colBounds.push({ first, last });
  }
  return { rowBounds, colBounds };
}

// Execute the movement instructions
function executePath(map, instructions, start) {
  const { rowBounds, colBounds } = getBoundaries(map);
  let { x, y } = start;
  let facing = 0; // Start facing right

  for (const instr of instructions) {
    if (instr === 'L' || instr === 'R') {
      facing = turn(facing, instr);
    } else {
      const steps = parseInt(instr, 10);
      for (let i = 0; i < steps; i++) {
        const nextPos = move(map, x, y, facing, rowBounds, colBounds);
        if (!nextPos) break; // Hit a wall
        ({ x, y } = nextPos);
      }
    }
  }

  return { x, y, facing };
}

// Handle turning
function turn(currentFacing, direction) {
  if (direction === 'R') {
    return (currentFacing + 1) % 4;
  } else if (direction === 'L') {
    return (currentFacing + 3) % 4; // Equivalent to -1 mod 4
  }
  return currentFacing;
}

// Move one step in the current facing direction, handling wrapping
function move(map, x, y, facing, rowBounds, colBounds) {
  let { dx, dy } = DIRECTIONS[facing];
  let newX = x + dx;
  let newY = y + dy;

  // Handle wrapping
  if (facing === 0 || facing === 2) { // Moving horizontally
    const row = rowBounds[y];
    if (newX < row.first) newX = row.last;
    if (newX > row.last) newX = row.first;
  } else { // Moving vertically
    const col = colBounds[x];
    if (newY < col.first) newY = col.last;
    if (newY > col.last) newY = col.first;
  }

  // Check boundaries
  if (newY >= map.length || newX >= map[newY].length || newY < 0 || newX < 0) {
    return null; // Out of bounds
  }

  // Check the target cell
  if (map[newY][newX] === '#') {
    return null; // Hit a wall
  } else {
    return { x: newX, y: newY };
  }
}

// Calculate the final password
function calculatePassword(position) {
  const row = position.y + 1;
  const column = position.x + 1;
  const facing = position.facing;
  return 1000 * row + 4 * column + facing;
}

// Main function
function main() {
  const { map, instructions } = readInput('input.txt');
  const start = findStart(map);
  const finalPosition = executePath(map, instructions, start);
  const password = calculatePassword(finalPosition);
  console.log(password);
}

main();
