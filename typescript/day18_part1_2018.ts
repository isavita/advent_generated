import * as fs from 'fs';

const inputFile = 'input.txt';
const width = 50;
const height = 50;

const readInput = (file: string): string[][] => {
    const data = fs.readFileSync(file, 'utf-8').trim().split('\n');
    return data.map(line => line.split(''));
};

const countAdjacent = (grid: string[][], x: number, y: number): { open: number; trees: number; lumberyards: number } => {
    const directions = [
        [-1, -1], [-1, 0], [-1, 1],
        [0, -1],          [0, 1],
        [1, -1], [1, 0], [1, 1]
    ];
    const counts = { open: 0, trees: 0, lumberyards: 0 };

    for (const [dx, dy] of directions) {
        const nx = x + dx, ny = y + dy;
        if (nx >= 0 && nx < width && ny >= 0 && ny < height) {
            const cell = grid[ny][nx];
            if (cell === '.') counts.open++;
            else if (cell === '|') counts.trees++;
            else if (cell === '#') counts.lumberyards++;
        }
    }
    return counts;
};

const nextState = (grid: string[][]): string[][] => {
    const newGrid = grid.map(row => [...row]);

    for (let y = 0; y < height; y++) {
        for (let x = 0; x < width; x++) {
            const counts = countAdjacent(grid, x, y);
            const current = grid[y][x];

            if (current === '.' && counts.trees >= 3) {
                newGrid[y][x] = '|';
            } else if (current === '|' && counts.lumberyards >= 3) {
                newGrid[y][x] = '#';
            } else if (current === '#' && (counts.lumberyards === 0 || counts.trees === 0)) {
                newGrid[y][x] = '.';
            }
        }
    }
    return newGrid;
};

const resourceValue = (grid: string[][]): number => {
    let trees = 0, lumberyards = 0;

    for (const row of grid) {
        for (const cell of row) {
            if (cell === '|') trees++;
            else if (cell === '#') lumberyards++;
        }
    }
    return trees * lumberyards;
};

const main = () => {
    let grid = readInput(inputFile);
    
    for (let minute = 0; minute < 10; minute++) {
        grid = nextState(grid);
    }
    
    const result = resourceValue(grid);
    console.log(result);
};

main();