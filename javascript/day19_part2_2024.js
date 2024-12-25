
const fs = require('fs');

function solve() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const [patternsLine, ...designLines] = input.split('\n');
    const patterns = patternsLine.split(', ').map(p => p.trim());
    const designs = designLines.filter(line => line.trim() !== '');

    let totalWays = 0;

    for (const design of designs) {
        const ways = countWays(design, patterns);
        totalWays += ways;
    }

    console.log(totalWays);
}

function countWays(design, patterns) {
    const memo = new Map();

    function recurse(remaining) {
        if (remaining === '') {
            return 1;
        }

        if (memo.has(remaining)) {
            return memo.get(remaining);
        }

        let count = 0;
        for (const pattern of patterns) {
            if (remaining.startsWith(pattern)) {
                count += recurse(remaining.substring(pattern.length));
            }
        }

        memo.set(remaining, count);
        return count;
    }

    return recurse(design);
}

solve();
