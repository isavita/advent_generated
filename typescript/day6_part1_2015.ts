const fs = require('fs');

const gridSize = 1000;

function processInstruction(instruction, grid) {
    const parts = instruction.split(' ');
    const [startX, startY] = parts[parts.length - 3].split(',').map(Number);
    const [endX, endY] = parts[parts.length - 1].split(',').map(Number);

    for (let x = startX; x <= endX; x++) {
        for (let y = startY; y <= endY; y++) {
            switch (true) {
                case instruction.startsWith('turn on'):
                    grid[x][y] = true;
                    break;
                case instruction.startsWith('turn off'):
                    grid[x][y] = false;
                    break;
                case instruction.startsWith('toggle'):
                    grid[x][y] = !grid[x][y];
                    break;
            }
        }
    }
}

function countLights(grid) {
    let count = 0;
    for (let row of grid) {
        for (let light of row) {
            if (light) {
                count++;
            }
        }
    }
    return count;
}

const file = fs.readFileSync('input.txt', 'utf8');
const grid = Array.from({ length: gridSize }, () => Array(gridSize).fill(false));

const instructions = file.split('\n');
for (let instruction of instructions) {
    processInstruction(instruction, grid);
}

console.log(countLights(grid));