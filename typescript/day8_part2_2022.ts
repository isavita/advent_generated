import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
const grid = input.map(line => line.split('').map(Number));
const rows = grid.length;
const cols = grid[0].length;

const isVisible = (row: number, col: number): boolean => {
    const height = grid[row][col];
    for (let i = 0; i < row; i++) if (grid[i][col] >= height) return false; // Up
    for (let i = row + 1; i < rows; i++) if (grid[i][col] >= height) return false; // Down
    for (let i = 0; i < col; i++) if (grid[row][i] >= height) return false; // Left
    for (let i = col + 1; i < cols; i++) if (grid[row][i] >= height) return false; // Right
    return true;
};

const countVisibleTrees = (): number => {
    let count = 0;
    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            if (r === 0 || r === rows - 1 || c === 0 || c === cols - 1 || isVisible(r, c)) {
                count++;
            }
        }
    }
    return count;
};

const viewingDistance = (row: number, col: number): number[] => {
    const height = grid[row][col];
    const distances = [0, 0, 0, 0]; // Up, Down, Left, Right

    for (let i = row - 1; i >= 0; i--) {
        distances[0]++;
        if (grid[i][col] >= height) break;
    }
    for (let i = row + 1; i < rows; i++) {
        distances[1]++;
        if (grid[i][col] >= height) break;
    }
    for (let i = col - 1; i >= 0; i--) {
        distances[2]++;
        if (grid[row][i] >= height) break;
    }
    for (let i = col + 1; i < cols; i++) {
        distances[3]++;
        if (grid[row][i] >= height) break;
    }

    return distances;
};

const highestScenicScore = (): number => {
    let maxScore = 0;
    for (let r = 1; r < rows - 1; r++) {
        for (let c = 1; c < cols - 1; c++) {
            const distances = viewingDistance(r, c);
            const score = distances.reduce((a, b) => a * b, 1);
            maxScore = Math.max(maxScore, score);
        }
    }
    return maxScore;
};

console.log(`Visible Trees: ${countVisibleTrees()}`);
console.log(`Highest Scenic Score: ${highestScenicScore()}`);