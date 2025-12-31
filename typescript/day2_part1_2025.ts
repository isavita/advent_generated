
import { readFileSync } from 'fs';

function sumInvalidIdsInRange(L: bigint, R: bigint): bigint {
    let total = 0n;
    for (let k = 1; ; k++) {
        const pow10 = 10n ** BigInt(k);
        const mul = pow10 + 1n;
        const minS = 10n ** BigInt(k - 1);
        const maxS = pow10 - 1n;
        const minX = minS * mul;
        if (minX > R) break;
        const sMin = (L + mul - 1n) / mul;
        const sMax = R / mul;
        const start = sMin > minS ? sMin : minS;
        const end = sMax < maxS ? sMax : maxS;
        if (start <= end) {
            const cnt = end - start + 1n;
            const sumS = cnt * (start + end) / 2n;
            total += sumS * mul;
        }
    }
    return total;
}

function main() {
    const path = 'input.txt';
    let content: string;
    try {
        content = readFileSync(path, 'utf8');
    } catch {
        return;
    }
    content = content.replace(/\s/g, '');
    if (!content) return;
    const ranges = content.split(',');
    let answer = 0n;
    for (const r of ranges) {
        if (!r) continue;
        const [lowStr, highStr] = r.split('-');
        if (!highStr) continue;
        const low = BigInt(lowStr);
        const high = BigInt(highStr);
        answer += sumInvalidIdsInRange(low, high);
    }
    console.log(answer.toString());
}

main();
