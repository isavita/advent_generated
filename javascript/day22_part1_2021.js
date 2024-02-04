const fs = require('fs');

class RebootStep {
    constructor(action, xStart, xEnd, yStart, yEnd, zStart, zEnd) {
        this.action = action;
        this.xStart = xStart;
        this.xEnd = xEnd;
        this.yStart = yStart;
        this.yEnd = yEnd;
        this.zStart = zStart;
        this.zEnd = zEnd;
    }
}

function parseRebootStep(line) {
    const parts = line.split(' ');

    const action = parts[0];
    const xRange = parts[1].split(',')[0].slice(2).split('..');
    const yRange = parts[1].split(',')[1].slice(2).split('..');
    const zRange = parts[1].split(',')[2].slice(2).split('..');

    const xStart = parseInt(xRange[0]);
    const xEnd = parseInt(xRange[1]);
    const yStart = parseInt(yRange[0]);
    const yEnd = parseInt(yRange[1]);
    const zStart = parseInt(zRange[0]);
    const zEnd = parseInt(zRange[1]);

    return new RebootStep(action, xStart, xEnd, yStart, yEnd, zStart, zEnd);
}

function createCubeGrid(minCoord, maxCoord) {
    const gridSize = maxCoord - minCoord + 1;
    const grid = new Array(gridSize).fill(null).map(() => new Array(gridSize).fill(null).map(() => new Array(gridSize).fill(false)));

    return grid;
}

function executeRebootSteps(cubeGrid, rebootSteps) {
    for (const step of rebootSteps) {
        if (!(step.xStart >= -50 && step.xEnd <= 50 && step.yStart >= -50 && step.yEnd <= 50 && step.zStart >= -50 && step.zEnd <= 50)) {
            continue;
        }
        for (let x = step.xStart; x <= step.xEnd; x++) {
            for (let y = step.yStart; y <= step.yEnd; y++) {
                for (let z = step.zStart; z <= step.zEnd; z++) {
                    cubeGrid[x + 50][y + 50][z + 50] = step.action === 'on';
                }
            }
        }
    }
}

function countOnCubes(cubeGrid) {
    let count = 0;

    for (const row of cubeGrid) {
        for (const col of row) {
            for (const cell of col) {
                if (cell) {
                    count++;
                }
            }
        }
    }

    return count;
}

const input = fs.readFileSync('input.txt', 'utf8').split('\n').filter(line => line.trim() !== '');
const rebootSteps = input.map(parseRebootStep);

const minCoord = -50;
const maxCoord = 50;
const cubeGrid = createCubeGrid(minCoord, maxCoord);
executeRebootSteps(cubeGrid, rebootSteps);
const onCubes = countOnCubes(cubeGrid);

console.log(onCubes);