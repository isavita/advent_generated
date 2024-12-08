
import * as fs from 'fs';

function solve() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const h = input.length;
    const w = input[0].length;
    const antennas: Map<string, Array<[number, number]>> = new Map();

    for (let y = 0; y < h; y++) {
        for (let x = 0; x < w; x++) {
            const c = input[y][x];
            if (c !== '.') {
                if (!antennas.has(c)) antennas.set(c, []);
                antennas.get(c)?.push([y, x]);
            }
        }
    }

    const antinodes: Set<string> = new Set();
    for (const coords of antennas.values()) {
        const n = coords.length;
        for (let i = 0; i < n; i++) {
            for (let j = i + 1; j < n; j++) {
                const [a0, a1] = coords[i];
                const [b0, b1] = coords[j];
                const p1 = [2 * a0 - b0, 2 * a1 - b1];
                const p2 = [2 * b0 - a0, 2 * b1 - a1];

                if (p1[0] >= 0 && p1[0] < h && p1[1] >= 0 && p1[1] < w) {
                    antinodes.add(JSON.stringify(p1));
                }
                if (p2[0] >= 0 && p2[0] < h && p2[1] >= 0 && p2[1] < w) {
                    antinodes.add(JSON.stringify(p2));
                }
            }
        }
    }
    console.log(antinodes.size);
}

solve();

