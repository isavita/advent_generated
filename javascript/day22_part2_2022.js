const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf-8');
const [mapInput, pathInput] = input.split('\n\n');

const mapLines = mapInput.split('\n');
const map = mapLines.map(line => line.split(''));

const path = pathInput.trim();

// Parse instructions
const instructions = [];
let num = '';
for (let i = 0; i < path.length; i++) {
    const c = path[i];
    if (c >= '0' && c <= '9') {
        num += c;
    } else if (c === 'L' || c === 'R') {
        if (num !== '') {
            instructions.push(parseInt(num));
            num = '';
        }
        instructions.push(c);
    }
}
if (num !== '') {
    instructions.push(parseInt(num));
}

// Pad the map so all lines have the same length
const maxWidth = Math.max(...mapLines.map(line => line.length));
for (let i = 0; i < map.length; i++) {
    while (map[i].length < maxWidth) {
        map[i].push(' ');
    }
}

// Directions: 0 = right, 1 = down, 2 = left, 3 = up
let row = 0;
let col = map[0].findIndex(c => c === '.');
let dir = 0; // Initially facing right

function getFace(row, col) {
    if (row >= 0 && row < 50 && col >= 50 && col < 100) {
        return 1;
    } else if (row >= 0 && row < 50 && col >= 100 && col < 150) {
        return 2;
    } else if (row >= 50 && row < 100 && col >= 50 && col < 100) {
        return 3;
    } else if (row >= 100 && row < 150 && col >= 0 && col < 50) {
        return 4;
    } else if (row >= 100 && row < 150 && col >= 50 && col < 100) {
        return 5;
    } else if (row >= 150 && row < 200 && col >= 0 && col < 50) {
        return 6;
    } else {
        return null;
    }
}

function wrap(row, col, dir) {
    const face = getFace(row, col);
    let newRow, newCol, newDir;
    if (face === 1) {
        if (dir === 3 && row === 0) { // Up from Face 1 to Face 6
            newRow = col - 50 + 150;
            newCol = 0;
            newDir = 0;
        } else if (dir === 2 && col === 50) { // Left from Face 1 to Face 4
            newRow = 149 - row;
            newCol = 0;
            newDir = 0;
        }
    } else if (face === 2) {
        if (dir === 3 && row === 0) { // Up from Face 2 to Face 6
            newRow = 199;
            newCol = col - 100;
            newDir = 3;
        } else if (dir === 0 && col === 149) { // Right from Face 2 to Face 5
            newRow = 149 - row;
            newCol = 99;
            newDir = 2;
        } else if (dir === 1 && row === 49) { // Down from Face 2 to Face 3
            newRow = col - 100 + 50;
            newCol = 99;
            newDir = 2;
        }
    } else if (face === 3) {
        if (dir === 0 && col === 99) { // Right from Face 3 to Face 2
            newRow = 49;
            newCol = row - 50 + 100;
            newDir = 3;
        } else if (dir === 2 && col === 50) { // Left from Face 3 to Face 4
            newRow = 100;
            newCol = row - 50;
            newDir = 1;
        }
    } else if (face === 4) {
        if (dir === 2 && col === 0) { // Left from Face 4 to Face 1
            newRow = 149 - row;
            newCol = 50;
            newDir = 0;
        } else if (dir === 3 && row === 100) { // Up from Face 4 to Face 3
            newRow = col + 50;
            newCol = 50;
            newDir = 0;
        }
    } else if (face === 5) {
        if (dir === 0 && col === 99) { // Right from Face 5 to Face 2
            newRow = 149 - row;
            newCol = 149;
            newDir = 2;
        } else if (dir === 1 && row === 149) { // Down from Face 5 to Face 6
            newRow = col - 50 + 150;
            newCol = 49;
            newDir = 2;
        }
    } else if (face === 6) {
        if (dir === 0 && col === 49) { // Right from Face 6 to Face 5
            newRow = 149;
            newCol = row - 150 + 50;
            newDir = 3;
        } else if (dir === 1 && row === 199) { // Down from Face 6 to Face 2
            newRow = 0;
            newCol = col + 100;
            newDir = 1;
        } else if (dir === 2 && col === 0) { // Left from Face 6 to Face 1
            newRow = 0;
            newCol = row - 150 + 50;
            newDir = 1;
        }
    }
    return { row: newRow, col: newCol, dir: newDir };
}

for (const instruction of instructions) {
    if (typeof instruction === 'number') {
        for (let step = 0; step < instruction; step++) {
            let newRow = row;
            let newCol = col;
            let newDir = dir;

            if (dir === 0) { // Right
                newCol++;
            } else if (dir === 1) { // Down
                newRow++;
            } else if (dir === 2) { // Left
                newCol--;
            } else if (dir === 3) { // Up
                newRow--;
            }

            if (newRow >= 0 && newRow < map.length && newCol >= 0 && newCol < map[0].length && map[newRow][newCol] !== ' ') {
                if (map[newRow][newCol] === '#') {
                    break;
                } else if (map[newRow][newCol] === '.') {
                    row = newRow;
                    col = newCol;
                }
            } else {
                const result = wrap(row, col, dir);
                newRow = result.row;
                newCol = result.col;
                newDir = result.dir;
                if (map[newRow][newCol] === '#') {
                    break;
                } else if (map[newRow][newCol] === '.') {
                    row = newRow;
                    col = newCol;
                    dir = newDir;
                } else {
                    throw new Error(`Invalid map at position (${newRow}, ${newCol})`);
                }
            }
        }
    } else if (instruction === 'L') {
        dir = (dir + 3) % 4; // Turn left
    } else if (instruction === 'R') {
        dir = (dir + 1) % 4; // Turn right
    }
}

const password = 1000 * (row + 1) + 4 * (col + 1) + dir;
console.log(password);
