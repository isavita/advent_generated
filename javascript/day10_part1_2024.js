
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf-8').trim();
const lines = input.split('\n');
const nr = lines.length;
const nc = lines[0].length;
const grid = lines.map(line => line.split('').map(Number));

const dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]];
const trailheads = [];
for (let r = 0; r < nr; r++) {
    for (let c = 0; c < nc; c++) {
        if (grid[r][c] === 0) {
            trailheads.push([r, c]);
        }
    }
}

let sumScores = 0;
for (const [tr, tc] of trailheads) {
    const reached = new Set();
    const front = [[tr, tc, 0]];
    const visited = new Set();

    while (front.length > 0) {
        const [r, c, h] = front.shift();

        if (h === 9) {
            reached.add(`${r},${c}`);
            continue;
        }

        for (const [dr, dc] of dirs) {
            const nr2 = r + dr;
            const nc2 = c + dc;
            if (nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc) continue;
            if (grid[nr2][nc2] === h + 1) {
                const key = `${nr2},${nc2},${h+1}`;
                if (!visited.has(key)) {
                    visited.add(key);
                    front.push([nr2, nc2, h + 1]);
                }
            }
        }
    }
    sumScores += reached.size;
}

console.log(sumScores);
