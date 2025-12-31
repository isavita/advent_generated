
import { readFileSync } from 'fs';

function main() {
    const data = readFileSync('input.txt', 'utf8').trimEnd();
    if (!data) {
        console.log('Total fresh IDs: 0');
        return;
    }
    const ranges: [bigint, bigint][] = [];
    for (const line of data.split('\n')) {
        const m = line.match(/^\s*(-?\d+)\s*-\s*(-?\d+)\s*$/);
        if (!m) continue;
        let a = BigInt(m[1]), b = BigInt(m[2]);
        if (a > b) [a, b] = [b, a];
        ranges.push([a, b]);
    }
    if (ranges.length === 0) {
        console.log('Total fresh IDs: 0');
        return;
    }
    ranges.sort((x, y) => (x[0] < y[0] ? -1 : x[0] > y[0] ? 1 : x[1] < y[1] ? -1 : x[1] > y[1] ? 1 : 0));
    let total = 0n;
    let [curL, curR] = ranges[0];
    for (let i = 1; i < ranges.length; i++) {
        const [l, r] = ranges[i];
        if (l <= curR) {
            if (r > curR) curR = r;
        } else {
            total += curR - curL + 1n;
            curL = l;
            curR = r;
        }
    }
    total += curR - curL + 1n;
    console.log(`Total fresh IDs: ${total}`);
}

main();
