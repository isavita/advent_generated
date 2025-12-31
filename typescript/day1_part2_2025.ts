
import { readFileSync } from 'fs';

function main() {
    const data = readFileSync('input.txt', 'utf8');
    let current = 50;
    let total = 0;
    for (const raw of data.split('\n')) {
        const line = raw.trim();
        if (!line) continue;
        const dir = line[0];
        const dist = parseInt(line.slice(1), 10);
        if (dir === 'R') {
            const next = current + dist;
            total += Math.floor(next / 100) - Math.floor(current / 100);
            current = next;
        } else if (dir === 'L') {
            const next = current - dist;
            total += Math.floor((current - 1) / 100) - Math.floor((next - 1) / 100);
            current = next;
        }
    }
    console.log(total);
}

main();
