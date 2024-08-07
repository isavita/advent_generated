import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n').map(line => line.split('').map(Number));

const directions = [
    [-1, -1], [-1, 0], [-1, 1],
    [0, -1],          [0, 1],
    [1, -1], [1, 0], [1, 1]
];

function simulateStep(grid: number[][]): number {
    const flashed = new Set<string>();
    const rows = grid.length;
    const cols = grid[0].length;

    const incrementEnergy = () => {
        for (let r = 0; r < rows; r++) {
            for (let c = 0; c < cols; c++) {
                grid[r][c]++;
            }
        }
    };

    const flashOctopus = (r: number, c: number) => {
        const key = `${r},${c}`;
        if (flashed.has(key) || grid[r][c] <= 9) return;
        flashed.add(key);
        for (const [dr, dc] of directions) {
            const nr = r + dr;
            const nc = c + dc;
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                grid[nr][nc]++;
                flashOctopus(nr, nc);
            }
        }
    };

    incrementEnergy();

    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            if (grid[r][c] > 9) flashOctopus(r, c);
        }
    }

    for (const key of flashed) {
        const [r, c] = key.split(',').map(Number);
        grid[r][c] = 0;
    }

    return flashed.size;
}

function totalFlashesAfterSteps(grid: number[][], steps: number): number {
    let totalFlashes = 0;
    for (let i = 0; i < steps; i++) {
        totalFlashes += simulateStep(grid);
    }
    return totalFlashes;
}

const totalFlashes = totalFlashesAfterSteps(input, 100);
console.log(totalFlashes);