
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let mask = '';
const memory = {};

const applyMask = (value, mask) => {
    let binaryValue = value.toString(2).padStart(36, '0');
    let result = '';
    for (let i = 0; i < mask.length; i++) {
        if (mask[i] === 'X') {
            result += binaryValue[i];
        } else {
            result += mask[i];
        }
    }
    return parseInt(result, 2);
};

for (let line of input) {
    if (line.startsWith('mask')) {
        mask = line.split(' = ')[1];
    } else {
        let [address, value] = line.match(/\d+/g).map(Number);
        memory[address] = applyMask(value, mask);
    }
}

let sum = Object.values(memory).reduce((acc, curr) => acc + curr, 0);
console.log(sum);
