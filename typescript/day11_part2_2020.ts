import * as fs from 'fs';

const directions = [
    [-1, -1], [-1, 0], [-1, 1],
    [0, -1],          [0, 1],
    [1, -1], [1, 0], [1, 1],
];

const readInput = (filename: string): string[][] => {
    const data = fs.readFileSync(filename, 'utf-8').trim().split('\n');
    return data.map(line => line.split(''));
};

const countAdjacentOccupied = (layout: string[][], row: number, col: number): number => {
    let count = 0;
    for (const [dx, dy] of directions) {
        const r = row + dx, c = col + dy;
        if (r >= 0 && r < layout.length && c >= 0 && c < layout[0].length && layout[r][c] === '#') {
            count++;
        }
    }
    return count;
};

const countVisibleOccupied = (layout: string[][], row: number, col: number): number => {
    let count = 0;
    for (const [dx, dy] of directions) {
        let r = row + dx, c = col + dy;
        while (r >= 0 && r < layout.length && c >= 0 && c < layout[0].length) {
            if (layout[r][c] === '#') {
                count++;
                break;
            }
            if (layout[r][c] === 'L') break;
            r += dx;
            c += dy;
        }
    }
    return count;
};

const simulateSeating = (layout: string[][], useVisibleRules: boolean): string[][] => {
    const newLayout = JSON.parse(JSON.stringify(layout));
    let changed = false;

    for (let row = 0; row < layout.length; row++) {
        for (let col = 0; col < layout[0].length; col++) {
            if (layout[row][col] === 'L' && (useVisibleRules ? countVisibleOccupied(layout, row, col) === 0 : countAdjacentOccupied(layout, row, col) === 0)) {
                newLayout[row][col] = '#';
                changed = true;
            } else if (layout[row][col] === '#' && (useVisibleRules ? countVisibleOccupied(layout, row, col) >= 5 : countAdjacentOccupied(layout, row, col) >= 4)) {
                newLayout[row][col] = 'L';
                changed = true;
            }
        }
    }
    return changed ? simulateSeating(newLayout, useVisibleRules) : newLayout;
};

const countOccupiedSeats = (layout: string[][]): number => {
    return layout.flat().filter(seat => seat === '#').length;
};

const main = () => {
    const layout = readInput('input.txt');
    const finalLayout = simulateSeating(layout, true);
    const occupiedCount = countOccupiedSeats(finalLayout);
    console.log(occupiedCount);
};

main();