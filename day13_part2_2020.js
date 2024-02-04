const fs = require('fs');
const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const buses = input[1].split(',').map((bus, index) => [bus, index]).filter(([bus]) => bus !== 'x');
const startTime = 100000000000000;

let timestamp = startTime;
let step = 1;

buses.forEach(([bus, index]) => {
    while ((timestamp + index) % bus !== 0) {
        timestamp += step;
    }
    step *= bus;
});

console.log(timestamp);