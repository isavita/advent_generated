import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim();
console.log(solve(input));

function solve(input: string): number {
    const matrix = parseInput(input);
    let originCol = 0;
    for (let i = 0; i < matrix[0].length; i++) {
        if (matrix[0][i] === '+') {
            originCol = i;
        }
        matrix[matrix.length - 1][i] = '#';
    }

    let ans = 0;
    while (!dropSand(matrix, originCol)) {
        ans++;
        if (matrix[0][originCol] === 'o') {
            break;
        }
    }

    return ans;
}

function parseInput(input: string): string[][] {
    const coordSets: [number, number][][] = [];
    let lowestCol = Number.MAX_SAFE_INTEGER;
    let highestRow = 0;
    for (const line of input.split('\n')) {
        const rawCoords = line.split(' -> ');
        const coords: [number, number][] = [];
        for (const rawCoord of rawCoords) {
            const rawNums = rawCoord.split(',');
            const col = parseInt(rawNums[0]);
            const row = parseInt(rawNums[1]);
            coords.push([col, row]);
            lowestCol = Math.min(lowestCol, col);
            highestRow = Math.max(highestRow, row);
        }
        coordSets.push(coords);
    }

    const ExtraLeftSpace = 200;
    let highestCol = 0;
    for (const set of coordSets) {
        for (const coord of set) {
            coord[0] -= lowestCol - ExtraLeftSpace;
            highestCol = Math.max(highestCol, coord[0]);
        }
    }

    const matrix: string[][] = Array.from({ length: highestRow + 3 }, () => Array(highestCol + ExtraLeftSpace * 2).fill('.'));

    for (const set of coordSets) {
        for (let i = 1; i < set.length; i++) {
            const cols = [set[i - 1][0], set[i][0]].sort((a, b) => a - b);
            const rows = [set[i - 1][1], set[i][1]].sort((a, b) => a - b);

            if (cols[0] === cols[1]) {
                for (let r = rows[0]; r <= rows[1]; r++) {
                    matrix[r][cols[0]] = '#';
                }
            } else if (rows[0] === rows[1]) {
                for (let c = cols[0]; c <= cols[1]; c++) {
                    matrix[rows[0]][c] = '#';
                }
            }
        }
    }

    const originCol = 500 - lowestCol + ExtraLeftSpace;
    matrix[0][originCol] = '+';

    return matrix;
}

function dropSand(matrix: string[][], originCol: number): boolean {
    let r = 0, c = originCol;

    while (r < matrix.length - 1) {
        const below = matrix[r + 1][c];
        const diagonallyLeft = matrix[r + 1][c - 1];
        const diagonallyRight = matrix[r + 1][c + 1];
        if (below === '.') {
            r++;
        } else if (diagonallyLeft === '.') {
            r++;
            c--;
        } else if (diagonallyRight === '.') {
            r++;
            c++;
        } else {
            matrix[r][c] = 'o';
            return false;
        }
    }

    return true;
}