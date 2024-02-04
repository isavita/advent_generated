const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim().split(' ').map(Number);

const parseTree = (data, index) => {
    let childCount = data[index];
    let metaCount = data[index + 1];
    index += 2;

    let childValues = [];
    for (let i = 0; i < childCount; i++) {
        let childValue;
        [childValue, index] = parseTree(data, index);
        childValues.push(childValue);
    }

    let value = 0;
    if (childCount === 0) {
        for (let i = 0; i < metaCount; i++) {
            value += data[index + i];
        }
    } else {
        for (let i = 0; i < metaCount; i++) {
            let metadata = data[index + i];
            if (metadata <= childCount && metadata > 0) {
                value += childValues[metadata - 1];
            }
        }
    }
    index += metaCount;

    return [value, index];
};

const [value, _] = parseTree(data, 0);
console.log(value);