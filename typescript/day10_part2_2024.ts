
import * as fs from 'fs';

type Pos = { r: number; c: number };

const dirs: Pos[] = [{ r: 1, c: 0 }, { r: -1, c: 0 }, { r: 0, c: 1 }, { r: 0, c: -1 }];

function solve() {
    const data = fs.readFileSync('input.txt', 'utf-8').trim();
    const lines = data.split('\n');
    const nr = lines.length;
    const nc = lines[0].length;
    const grid: number[][] = lines.map(line => line.split('').map(Number));

    const dp: (bigint | null)[][] = Array(nr).fill(null).map(() => Array(nc).fill(null));

    const dfs = (r: number, c: number): bigint => {
        if (dp[r][c] !== null) {
            return dp[r][c]!;
        }
        const h = grid[r][c];
        if (h === 9) {
            dp[r][c] = 1n;
            return 1n;
        }
        let sum = 0n;
        for (const d of dirs) {
            const nr2 = r + d.r;
            const nc2 = c + d.c;
            if (nr2 < 0 || nr2 >= nr || nc2 < 0 || nc2 >= nc) continue;
            if (grid[nr2][nc2] === h + 1) {
                sum += dfs(nr2, nc2);
            }
        }
        dp[r][c] = sum;
        return sum;
    };

    let total = 0n;
    for (let r = 0; r < nr; r++) {
        for (let c = 0; c < nc; c++) {
            if (grid[r][c] === 0) {
                total += dfs(r, c);
            }
        }
    }
    console.log(total.toString());
}

solve();
