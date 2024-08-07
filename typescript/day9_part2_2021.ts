import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n').map(line => line.split('').map(Number));
const rows = input.length;
const cols = input[0].length;

const directions = [
    [-1, 0], // up
    [1, 0],  // down
    [0, -1], // left
    [0, 1]   // right
];

const isLowPoint = (x: number, y: number): boolean => {
    const height = input[x][y];
    return directions.every(([dx, dy]) => {
        const nx = x + dx, ny = y + dy;
        return nx < 0 || nx >= rows || ny < 0 || ny >= cols || input[nx][ny] > height;
    });
};

const getBasinSize = (x: number, y: number, visited: boolean[][]): number => {
    if (x < 0 || x >= rows || y < 0 || y >= cols || visited[x][y] || input[x][y] === 9) return 0;
    visited[x][y] = true;
    let size = 1;
    for (const [dx, dy] of directions) {
        size += getBasinSize(x + dx, y + dy, visited);
    }
    return size;
};

const lowPoints: number[] = [];
const visited = Array.from({ length: rows }, () => Array(cols).fill(false));

for (let i = 0; i < rows; i++) {
    for (let j = 0; j < cols; j++) {
        if (isLowPoint(i, j)) {
            lowPoints.push(input[i][j] + 1);
        }
    }
}

const riskLevelSum = lowPoints.reduce((sum, level) => sum + level, 0);
console.log('Sum of risk levels:', riskLevelSum);

const basinSizes: number[] = [];

for (let i = 0; i < rows; i++) {
    for (let j = 0; j < cols; j++) {
        if (!visited[i][j] && input[i][j] !== 9) {
            basinSizes.push(getBasinSize(i, j, visited));
        }
    }
}

basinSizes.sort((a, b) => b - a);
const largestBasinsProduct = basinSizes.slice(0, 3).reduce((product, size) => product * size, 1);
console.log('Product of the sizes of the three largest basins:', largestBasinsProduct);