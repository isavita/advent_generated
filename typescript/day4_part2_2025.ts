
import * as fs from 'fs';

function main() {
    const data = fs.readFileSync('input.txt', 'utf8').trimEnd().split('\n');
    if (data.length === 0) return;
    const rows = data.length;
    const cols = data[0].length;
    const grid: string[][] = data.map(line => line.split(''));

    let totalRemoved = 0;
    while (true) {
        const toRemove: [number, number][] = [];
        for (let r = 0; r < rows; r++) {
            for (let c = 0; c < cols; c++) {
                if (grid[r][c] !== '@') continue;
                let cnt = 0;
                for (let dr = -1; dr <= 1; dr++) {
                    for (let dc = -1; dc <= 1; dc++) {
                        if (dr === 0 && dc === 0) continue;
                        const nr = r + dr, nc = c + dc;
                        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] === '@') cnt++;
                    }
                }
                if (cnt < 4) toRemove.push([r, c]);
            }
        }
        if (toRemove.length === 0) break;
        totalRemoved += toRemove.length;
        for (const [r, c] of toRemove) grid[r][c] = '.';
    }
    console.log(totalRemoved);
}

main();
