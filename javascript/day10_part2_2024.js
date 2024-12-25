
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf-8').trim();
const lines = input.split('\n');
const nr = lines.length;
const nc = lines[0].length;
const grid = lines.map(line => line.split('').map(Number));
const dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]];
const dp = Array(nr).fill(null).map(() => Array(nc).fill(-1));

function dfs(r, c) {
    if (dp[r][c] !== -1) return dp[r][c];
    const h = grid[r][c];
    if (h === 9) return dp[r][c] = 1;
    let sum = 0;
    for (const [dr, dc] of dirs) {
        const nr2 = r + dr;
        const nc2 = c + dc;
        if (nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc) continue;
        if (grid[nr2][nc2] === h + 1) {
            sum += dfs(nr2, nc2);
        }
    }
    return dp[r][c] = sum;
}

let total = 0;
for (let r = 0; r < nr; r++) {
    for (let c = 0; c < nc; c++) {
        if (grid[r][c] === 0) {
            total += dfs(r, c);
        }
    }
}

console.log(total);
