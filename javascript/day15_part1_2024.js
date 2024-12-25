
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf-8').trim();
const lines = input.split('\n');

let grid = [];
let moves = '';
let readingMap = true;

for (const line of lines) {
    if (readingMap) {
        if (line.includes('#')) {
            grid.push(line);
        } else {
            readingMap = false;
            moves += line;
        }
    } else {
        moves += line;
    }
}

const rows = grid.length;
const cols = grid[0].length;
const runes = grid.map(row => row.split(''));

let robotR, robotC;
for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
        if (runes[r][c] === '@') {
            robotR = r;
            robotC = c;
            break;
        }
    }
}

const dirs = {
    '^': [-1, 0],
    'v': [1, 0],
    '<': [0, -1],
    '>': [0, 1],
};

function pushBoxes(r, c, dr, dc) {
    let nr = r + dr;
    let nc = c + dc;
    if (runes[nr][nc] === '#') {
        return false;
    }
    if (runes[nr][nc] === 'O') {
        if (!pushBoxes(nr, nc, dr, dc)) {
            return false;
        }
    }
    if (runes[nr][nc] === '.') {
        runes[nr][nc] = 'O';
        runes[r][c] = '.';
        return true;
    }
    return false;
}

for (const move of moves) {
    const d = dirs[move];
    const nr = robotR + d[0];
    const nc = robotC + d[1];
    if (runes[nr][nc] === '#') {
        continue;
    } else if (runes[nr][nc] === 'O') {
        if (!pushBoxes(nr, nc, d[0], d[1])) {
            continue;
        }
    }
    if (runes[nr][nc] === '.' || runes[nr][nc] === 'O') {
        runes[robotR][robotC] = '.';
        runes[nr][nc] = '@';
        robotR = nr;
        robotC = nc;
    }
}

let sum = 0;
for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
        if (runes[r][c] === 'O') {
            sum += r * 100 + c;
        }
    }
}

console.log(sum);
