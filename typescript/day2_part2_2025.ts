import * as fs from 'fs';

function isRepeatingPattern(s: string, d: number): boolean {
    const pattern = s.slice(0, d);
    for (let i = d; i < s.length; i += d) {
        if (s.slice(i, i + d) !== pattern) return false;
    }
    return true;
}

function isInvalid(n: bigint): boolean {
    const s = n.toString();
    const len = s.length;
    for (let d = 1; d <= len >> 1; d++) {
        if (len % d === 0 && isRepeatingPattern(s, d)) return true;
    }
    return false;
}

function main() {
    let input: string;
    try {
        input = fs.readFileSync('input.txt', 'utf8');
    } catch {
        return;
    }
    const cleaned = input.replace(/\s+/g, '');
    if (!cleaned) return;
    const ranges = cleaned.split(',');
    const unique = new Set<bigint>();
    for (const range of ranges) {
        if (!range) continue;
        const parts = range.split('-');
        if (parts.length !== 2) continue;
        let start: bigint, end: bigint;
        try {
            start = BigInt(parts[0]);
            end = BigInt(parts[1]);
        } catch {
            continue;
        }
        for (let cur = start; cur <= end; cur++) {
            if (isInvalid(cur)) unique.add(cur);
        }
    }
    let sum = 0n;
    for (const v of unique) sum += v;
    console.log(sum.toString());
}

main();