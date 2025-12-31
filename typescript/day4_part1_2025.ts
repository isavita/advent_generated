
import * as fs from 'fs';

function main() {
    const lines = fs.readFileSync('input.txt', 'utf8')
        .split(/\r?\n/)
        .filter(l => l.trim().length > 0);

    if (lines.length === 0) {
        console.log(0);
        return;
    }

    const rows = lines.length;
    const cols = lines[0].length;
    let total = 0;
    const offsets = [-1, 0, 1];

    for (let r = 0; r < rows; r++) {
        const row = lines[r];
        for (let c = 0; c < cols; c++) {
            if (row[c] !== '@') continue;
            let neigh = 0;
            for (const dr of offsets) {
                for (const dc of offsets) {
                    if (dr === 0 && dc === 0) continue;
                    const nr = r + dr, nc = c + dc;
                    if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && lines[nr][nc] === '@') {
                        if (++neigh >= 4) break;
                    }
                }
                if (neigh >= 4) break;
            }
            if (neigh < 4) total++;
        }
    }

    console.log(total);
}

main();
