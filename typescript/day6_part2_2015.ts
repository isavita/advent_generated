const fs = require('fs');

const gridSize = 1000;

const grid = Array.from({ length: gridSize }, () => Array(gridSize).fill(0));

const data = fs.readFileSync('input.txt', 'utf8').split('\n');

data.forEach(instruction => processInstruction(instruction, grid));

console.log(totalBrightness(grid));

function processInstruction(instruction, grid) {
    const parts = instruction.split(' ');
    const [startX, startY] = parts[parts.length - 3].split(',').map(Number);
    const [endX, endY] = parts[parts.length - 1].split(',').map(Number);

    for (let x = startX; x <= endX; x++) {
        for (let y = startY; y <= endY; y++) {
            switch (true) {
                case instruction.startsWith('turn on'):
                    grid[x][y]++;
                    break;
                case instruction.startsWith('turn off'):
                    if (grid[x][y] > 0) {
                        grid[x][y]--;
                    }
                    break;
                case instruction.startsWith('toggle'):
                    grid[x][y] += 2;
                    break;
            }
        }
    }
}

function totalBrightness(grid) {
    return grid.flat().reduce((acc, curr) => acc + curr, 0);
}