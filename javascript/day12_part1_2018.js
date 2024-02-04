
const fs = require('fs');

const inputFile = 'input.txt';

const fileData = fs.readFileSync(inputFile, 'utf8').split('\n');

let initialState = '';
const rules = new Map();

fileData.forEach(line => {
    if (line.includes('initial state')) {
        initialState = line.split(': ')[1];
    } else if (line.includes('=>')) {
        const parts = line.split(' => ');
        rules.set(parts[0], parts[1][0]);
    }
});

const state = new Map();
initialState.split('').forEach((c, i) => {
    if (c === '#') {
        state.set(i, '#');
    }
});

for (let generation = 0; generation < 20; generation++) {
    const newState = new Map();
    const [minPot, maxPot] = minMaxKeys(state);
    for (let i = minPot - 2; i <= maxPot + 2; i++) {
        let pattern = '';
        for (let j = i - 2; j <= i + 2; j++) {
            if (state.get(j) === '#') {
                pattern += '#';
            } else {
                pattern += '.';
            }
        }
        if (rules.get(pattern) === '#') {
            newState.set(i, '#');
        }
    }
    state.clear();
    newState.forEach((value, key) => {
        state.set(key, value);
    });
}

let sum = 0;
state.forEach((value, key) => {
    sum += key;
});

console.log(sum);

function minMaxKeys(m) {
    let minKey, maxKey;
    let first = true;
    for (const k of m.keys()) {
        if (first) {
            minKey = k;
            maxKey = k;
            first = false;
        } else {
            if (k < minKey) {
                minKey = k;
            }
            if (k > maxKey) {
                maxKey = k;
            }
        }
    }
    return [minKey, maxKey];
}
