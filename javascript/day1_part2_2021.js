let fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(Number);

let largerThanPrevious = 0;
for (let i = 1; i < input.length; i++) {
    if (input[i] > input[i - 1]) {
        largerThanPrevious++;
    }
}

console.log(largerThanPrevious);

let slidingWindowLarger = 0;
for (let i = 2; i < input.length; i++) {
    if (input[i] + input[i - 1] + input[i - 2] > input[i - 1] + input[i - 2] + input[i - 3]) {
        slidingWindowLarger++;
    }
}

console.log(slidingWindowLarger);