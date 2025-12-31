
import { readFileSync } from 'fs';

function largestSubseq(s: string, k: number): string {
    const n = s.length;
    let toRemove = n - k;
    const stack: string[] = [];
    for (let i = 0; i < n; i++) {
        const c = s[i];
        while (toRemove && stack.length && stack[stack.length - 1] < c) {
            stack.pop();
            toRemove--;
        }
        stack.push(c);
    }
    return stack.slice(0, k).join('');
}

const data = readFileSync('input.txt', 'utf8')
    .split('\n')
    .map(l => l.trim())
    .filter(Boolean);

let total = 0n;
for (const line of data) {
    const val = BigInt(largestSubseq(line, 12));
    total += val;
}
console.log(total.toString());
