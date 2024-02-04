const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim().split(' ').map(Number);
let index = 0;

const parseTree = () => {
    const childCount = data[index];
    const metaCount = data[index + 1];
    index += 2;

    let sum = 0;
    for (let i = 0; i < childCount; i++) {
        sum += parseTree();
    }

    for (let i = 0; i < metaCount; i++) {
        sum += data[index + i];
    }
    index += metaCount;

    return sum;
};

const result = parseTree();
console.log(result);