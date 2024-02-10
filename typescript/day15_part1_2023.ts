const fs = require('fs');

const hashTableSize = 256;

class Step {
    constructor(label, numBox, operation, number) {
        this.label = label;
        this.numBox = numBox;
        this.operation = operation;
        this.number = number;
    }
}

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
    let step = new Step();

    step.label = stepStr.replace(/=-0123456789/g, '');
    step.numBox = hashString(step.label);
    step.operation = stepStr.charAt(step.label.length);
    if (step.operation === '=') {
        step.number = parseInt(stepStr.slice(step.label.length + 1));
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