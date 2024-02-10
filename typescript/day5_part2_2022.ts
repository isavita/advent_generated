const fs = require('fs');

function readAll(path) {
    const file = fs.readFileSync(path, 'utf8');
    return file.trim();
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
        stacks[toIndex] = stacks[toIndex].concat(stacks[fromIndex].slice(-n));
        stacks[fromIndex] = stacks[fromIndex].slice(0, -n);
    }

    const result = new Array(stacks.length);
    for (let i = 0; i < stacks.length; i++) {
        result[i] = stacks[i][stacks[i].length - 1];
    }

    return result.join('');
}

const input = readAll('input.txt').split('\n\n');
const stacks = new Array((input[0].split('\n')[0].length + 1) / 4);
input[0].split('\n').forEach(line => {
    for (let i = 0; i < line.length; i++) {
        const b = line.charCodeAt(i);
        if (b >= 65 && b <= 90) {
            stacks[(i - 1) / 4] = (stacks[(i - 1) / 4] || '') + String.fromCharCode(b);
        }
    }
});

const steps = input[1].split('\n');
console.log(move(stacks, steps));