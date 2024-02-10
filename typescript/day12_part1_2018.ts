
const fs = require('fs');

const file = fs.readFileSync('input.txt', 'utf8');
const lines = file.split('\n');

let initialState = '';
const rules = new Map();

for (const line of lines) {
    if (line.includes('initial state')) {
        initialState = line.split(': ')[1];
    } else if (line.includes('=>')) {
        const parts = line.split(' => ');
        rules.set(parts[0], parts[1][0]);
    }
}

const state = new Map();
for (let i = 0; i < initialState.length; i++) {
    if (initialState[i] === '#') {
        state.set(i, '#');
    }
}

for (let generation = 0; generation < 20; generation++) {
    const newState = new Map();
    const [minPot, maxPot] = minMaxKeys(state);
    for (let i = minPot - 2; i <= maxPot + 2; i++) {
        let pattern = '';
        for (let j = i - 2; j <= i + 2; j++) {
            pattern += state.get(j) === '#' ? '#' : '.';
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
    let first = true;
    let minKey = 0;
    let maxKey = 0;
    for (const key of m.keys()) {
        if (first) {
            minKey = key;
            maxKey = key;
            first = false;
        } else {
            if (key < minKey) {
                minKey = key;
            }
            if (key > maxKey) {
                maxKey = key;
            }
        }
    }
    return [minKey, maxKey];
}
