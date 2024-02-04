const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split(',');

let sum = 0;

const HASH = (str) => {
    let current = 0;
    for (let i = 0; i < str.length; i++) {
        const charCode = str.charCodeAt(i);
        current += charCode;
        current *= 17;
        current %= 256;
    }
    return current;
};

const boxes = new Array(256).fill().map(() => []);

for (let step of input) {
    const [label, operation] = step.split(/[=-]/);
    const boxIndex = HASH(label);
    if (operation === '') {
        boxes[boxIndex] = boxes[boxIndex].filter(lens => lens.label !== label);
    } else {
        const focalLength = parseInt(operation);
        const lensIndex = boxes[boxIndex].findIndex(lens => lens.label === label);
        if (lensIndex !== -1) {
            boxes[boxIndex][lensIndex].focalLength = focalLength;
        } else {
            boxes[boxIndex].push({ label, focalLength });
        }
    }
}

let totalFocusingPower = 0;

for (let i = 0; i < 256; i++) {
    for (let j = 0; j < boxes[i].length; j++) {
        const lens = boxes[i][j];
        totalFocusingPower += (i + 1) * (j + 1) * lens.focalLength;
    }
}

console.log(totalFocusingPower);