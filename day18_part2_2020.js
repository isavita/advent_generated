const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();
const lines = input.split('\n');

let total = 0;

for (const line of lines) {
    total += doMaths(parseInput(line), calcFlatSlicePart);
}

console.log(total);

function parseInput(input) {
    return input.split('').filter(char => char !== ' ').map(char => char);
}

function doMaths(input, flatteningFunc) {
    const stackOpenIndices = [];
    let stackFlattened = [];

    for (let i = 0; i < input.length; i++) {
        stackFlattened.push(input[i]);

        switch (input[i]) {
            case '(':
                stackOpenIndices.push(stackFlattened.length - 1);
                break;
            case ')':
                const openIndex = stackOpenIndices.pop();
                const sliToFlatten = stackFlattened.slice(openIndex + 1, stackFlattened.length - 1);
                stackFlattened[openIndex] = flatteningFunc(sliToFlatten);
                stackFlattened = stackFlattened.slice(0, openIndex + 1);
                break;
        }
    }

    return toInt(flatteningFunc(stackFlattened));
}

function calcFlatSlicePart(input) {
    for (let i = 0; i < input.length; i++) {
        if (input[i] === '(' || input[i] === ')') {
            throw new Error(`unexpected paren in flat input, ${input}`);
        }
    }

    for (let i = 1; i < input.length - 1; i++) {
        if (input[i] === '+') {
            const toLeft = input[i - 1];
            const toRight = input[i + 1];
            if (!isNaN(toLeft) && !isNaN(toRight)) {
                input[i - 1] = addStrings(toLeft, toRight);
                input.splice(i, 2);
                i--;
            }
        }
    }

    for (let i = 1; i < input.length - 1; i++) {
        if (input[i] === '*') {
            const toLeft = input[i - 1];
            const toRight = input[i + 1];
            if (!isNaN(toLeft) && !isNaN(toRight)) {
                input[i - 1] = multiplyStrings(toLeft, toRight);
                input.splice(i, 2);
                i--;
            }
        }
    }

    return input[0];
}

function addStrings(...strs) {
    let sum = 0;
    for (const str of strs) {
        sum += parseInt(str, 10);
    }
    return sum.toString();
}

function multiplyStrings(...strs) {
    let product = 1;
    for (const str of strs) {
        product *= parseInt(str, 10);
    }
    return product.toString();
}

function toInt(s) {
    const n = parseInt(s, 10);
    if (isNaN(n)) {
        throw new Error('Invalid number');
    }
    return n;
}