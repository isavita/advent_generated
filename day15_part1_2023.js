const fs = require('fs');

const hashTableSize = 256;

function hashString(str) {
    let res = 0;
    for (let i = 0; i < str.length; i++) {
        let char = str.charCodeAt(i);
        res += char;
        res *= 17;
        res %= hashTableSize;
    }
    return res;
}

function parseStep(stepStr) {
    let step = {};

    step.Label = stepStr.replace(/=-0123456789/g, '');
    step.NumBox = hashString(step.Label);
    step.Operation = stepStr.substr(step.Label.length, 1);
    if (step.Operation === '=') {
        step.Number = parseInt(stepStr.substr(step.Label.length + 1));
    }

    return step;
}

function solve(input) {
    let line = input[0];
    let steps = line.split(',');
    let res = 0;
    for (let step of steps) {
        res += hashString(step);
    }
    return res;
}

function readFile(fileName) {
    return fs.readFileSync(fileName, 'utf8').trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));