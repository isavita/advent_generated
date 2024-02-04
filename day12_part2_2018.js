
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

let initialState = '';
let rules = {};

for (let i = 0; i < input.length; i++) {
    if (input[i].includes("initial state")) {
        initialState = input[i].split(': ')[1];
    } else if (input[i].includes("=>")) {
        const parts = input[i].split(' => ');
        rules[parts[0]] = parts[1][0];
    }
}

let state = {};
for (let i = 0; i < initialState.length; i++) {
    if (initialState[i] === '#') {
        state[i] = '#';
    }
}

let previousPattern = "";
let previousSum = 0;
let offset = 0;
for (let generation = 0; generation < 50000000000; generation++) {
    let newState = {};
    let [minPot, maxPot] = minMaxKeys(state);
    for (let i = minPot - 2; i <= maxPot + 2; i++) {
        let pattern = "";
        for (let j = i - 2; j <= i + 2; j++) {
            if (state[j] === '#') {
                pattern += "#";
            } else {
                pattern += ".";
            }
        }
        if (rules[pattern] === '#') {
            newState[i] = '#';
        }
    }
    state = newState;

    let [currentPattern, currentSum] = statePattern(state);
    if (currentPattern === previousPattern) {
        offset = currentSum - previousSum;
        let remainingGenerations = 50000000000 - generation - 1;
        let finalSum = currentSum + offset * remainingGenerations;
        console.log(finalSum);
        process.exit();
    }
    previousPattern = currentPattern;
    previousSum = currentSum;
}

function minMaxKeys(m) {
    let minKey, maxKey;
    let keys = Object.keys(m);
    minKey = maxKey = keys[0];
    for (let i = 1; i < keys.length; i++) {
        let k = parseInt(keys[i]);
        if (k < minKey) minKey = k;
        if (k > maxKey) maxKey = k;
    }
    return [parseInt(minKey), parseInt(maxKey)];
}

function statePattern(m) {
    let [minPot, maxPot] = minMaxKeys(m);
    let pattern = '';
    let sum = 0;
    for (let i = minPot; i <= maxPot; i++) {
        if (m[i] === '#') {
            pattern += '#';
            sum += i;
        } else {
            pattern += '.';
        }
    }
    return [pattern, sum];
}
