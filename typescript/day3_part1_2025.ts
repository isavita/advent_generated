
import { readFileSync } from 'fs';

function maxJoltage(s: string): number {
    for (let t = 9; t >= 1; t--) {
        const idx = s.indexOf(t.toString());
        if (idx !== -1 && idx < s.length - 1) {
            let maxU = 0;
            for (let i = idx + 1; i < s.length; i++) {
                const d = s.charCodeAt(i) - 48;
                if (d > maxU) maxU = d;
            }
            return t * 10 + maxU;
        }
    }
    return 0;
}

function main() {
    const data = readFileSync('input.txt', 'utf8').split('\n');
    let total = 0;
    for (let line of data) {
        line = line.trim();
        if (line) total += maxJoltage(line);
    }
    console.log(total);
}

main();
