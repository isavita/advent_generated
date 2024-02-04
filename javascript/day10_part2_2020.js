const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(Number);

const adapters = [0, ...input].sort((a, b) => a - b);
adapters.push(adapters[adapters.length - 1] + 3);

console.log(countArrangements(adapters));

function countArrangements(adapters) {
    const ways = {};
    ways[0] = 1;

    for (let i = 1; i < adapters.length; i++) {
        const currentJoltage = adapters[i];
        ways[currentJoltage] = 0;
        for (const diff of [1, 2, 3]) {
            ways[currentJoltage] += ways[currentJoltage - diff] || 0;
        }
    }

    return ways[adapters[adapters.length - 1]];
}