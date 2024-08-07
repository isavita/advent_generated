import * as fs from 'fs';

const inputFilePath = 'input.txt';

function readInput(filePath: string): string[][] {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.trim().split('\n').map(line => line.split(''));
}

function countAdjacentBugs(grid: string[][], x: number, y: number): number {
    const directions = [
        [-1, 0], [1, 0], [0, -1], [0, 1]
    ];
    return directions.reduce((count, [dx, dy]) => {
        const nx = x + dx, ny = y + dy;
        if (nx >= 0 && nx < 5 && ny >= 0 && ny < 5 && grid[nx][ny] === '#') {
            count++;
        }
        return count;
    }, 0);
}

function nextGridState(grid: string[][]): string[][] {
    return grid.map((row, x) => 
        row.map((cell, y) => {
            const adjacentBugs = countAdjacentBugs(grid, x, y);
            if (cell === '#') {
                return (adjacentBugs === 1) ? '#' : '.';
            } else {
                return (adjacentBugs === 1 || adjacentBugs === 2) ? '#' : '.';
            }
        })
    );
}

function gridToString(grid: string[][]): string {
    return grid.map(row => row.join('')).join('');
}

function biodiversityRating(grid: string[][]): number {
    return grid.flatMap(row => row)
        .reduce((rating, cell, index) => {
            return cell === '#' ? rating + Math.pow(2, index) : rating;
        }, 0);
}

function main() {
    let grid = readInput(inputFilePath);
    const seenLayouts = new Set<string>();

    while (true) {
        const layoutStr = gridToString(grid);
        if (seenLayouts.has(layoutStr)) {
            console.log(biodiversityRating(grid));
            break;
        }
        seenLayouts.add(layoutStr);
        grid = nextGridState(grid);
    }
}

main();