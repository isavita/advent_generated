const fs = require('fs');

function readAll(path) {
    return fs.readFileSync(path, 'utf8').trim();
}

function move(st, steps) {
    const stacks = new Array(st.length);
    for (let i = 0; i < st.length; i++) {
        stacks[i] = new Array(st[i].length);
        for (let j = 0; j < st[i].length; j++) {
            stacks[i][j] = st[i][st[i].length - j - 1];
        }
    }

    for (let step of steps) {
        const [, n, from, to] = step.match(/move (\d+) from (\d+) to (\d+)/).map(Number);
        const fromIndex = from - 1;
        const toIndex = to - 1;
        for (let i = 0; i < n; i++) {
            stacks[toIndex].push(stacks[fromIndex][stacks[fromIndex].length - 1]);
            stacks[fromIndex].pop();
        }
    }

    const result = new Array(stacks.length);
    for (let i = 0; i < stacks.length; i++) {
        result[i] = stacks[i][stacks[i].length - 1];
    }

    return result.join('');
}

const input = readAll('input.txt').split('\n\n');
const inputLines = input[0].split('\n');
const stacks = new Array(Math.floor((inputLines[0].length + 1) / 4));
for (let line of inputLines) {
    for (let i = 0; i < line.length; i++) {
        const b = line.charCodeAt(i);
        if (b >= 65 && b <= 90) {
            stacks[Math.floor((i - 1) / 4)] = [...(stacks[Math.floor((i - 1) / 4)] || []), String.fromCharCode(b)];
        }
    }
}

const steps = input[1].split('\n');
console.log(move(stacks, steps));