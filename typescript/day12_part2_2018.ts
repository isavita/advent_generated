
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
initialState.split('').forEach((c, i) => {
    if (c === '#') {
        state.set(i, '#');
    }
});

let previousPattern = '';
let previousSum = 0;
let offset = 0;

for (let generation = 0; generation < 50000000000; generation++) {
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
    newState.forEach((val, key) => {
        state.set(key, val);
    });

    const [currentPattern, currentSum] = statePattern(state);
    if (currentPattern === previousPattern) {
        offset = currentSum - previousSum;
        const remainingGenerations = 50000000000 - generation - 1;
        const finalSum = currentSum + offset * remainingGenerations;
        console.log(finalSum);
        process.exit();
    }
    previousPattern = currentPattern;
    previousSum = currentSum;
}

function minMaxKeys(m) {
    let minKey = 0;
    let maxKey = 0;
    let first = true;
    m.forEach((val, key) => {
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
    });
    return [minKey, maxKey];
}

function statePattern(m) {
    const keys = Array.from(m.keys());
    const minPot = Math.min(...keys);
    const maxPot = Math.max(...keys);
    let pattern = '';
    let sum = 0;
    for (let i = minPot; i <= maxPot; i++) {
        if (m.get(i) === '#') {
            pattern += '#';
            sum += i;
        } else {
            pattern += '.';
        }
    }
    return [pattern, sum];
}
