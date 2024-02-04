const fs = require('fs');

const gridSize = 1000;

const grid = new Array(gridSize).fill(0).map(() => new Array(gridSize).fill(0));

const data = fs.readFileSync('input.txt', 'utf8').split('\n');

data.forEach(instruction => {
    processInstruction(instruction, grid);
});

console.log(totalBrightness(grid));

function processInstruction(instruction, grid) {
    const parts = instruction.split(' ');
    const [startX, startY] = parts[parts.length - 3].split(',').map(Number);
    const [endX, endY] = parts[parts.length - 1].split(',').map(Number);

    for (let x = startX; x <= endX; x++) {
        for (let y = startY; y <= endY; y++) {
            if (instruction.startsWith('turn on')) {
                grid[x][y]++;
            } else if (instruction.startsWith('turn off')) {
                if (grid[x][y] > 0) {
                    grid[x][y]--;
                }
            } else if (instruction.startsWith('toggle')) {
                grid[x][y] += 2;
            }
        }
    }
}

function totalBrightness(grid) {
    let brightness = 0;
    grid.forEach(row => {
        row.forEach(light => {
            brightness += light;
        });
    });
    return brightness;
}