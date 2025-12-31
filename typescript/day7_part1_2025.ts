
import * as fs from 'fs';

function main() {
    const lines = fs.readFileSync('input.txt', 'utf8').trimEnd().split('\n');
    if (lines.length === 0) { console.log(0); return; }

    let startRow = -1, startCol = -1;
    for (let r = 0; r < lines.length; r++) {
        const c = lines[r].indexOf('S');
        if (c !== -1) { startRow = r; startCol = c; break; }
    }
    if (startRow === -1) { console.log(0); return; }

    let beams = new Set<number>([startCol]);
    let splits = 0;

    for (let r = startRow + 1; r < lines.length; r++) {
        const row = lines[r];
        const next = new Set<number>();
        for (const c of beams) {
            if (c >= 0 && c < row.length) {
                if (row[c] === '^') {
                    splits++;
                    next.add(c - 1);
                    next.add(c + 1);
                } else {
                    next.add(c);
                }
            }
        }
        beams = next;
        if (beams.size === 0) break;
    }

    console.log(splits);
}

main();
