const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\t').map(Number);

const seenConfigs = new Set();
let cycles = 0;

while (!seenConfigs.has(input.join(','))) {
    seenConfigs.add(input.join(','));
    
    let maxIndex = input.indexOf(Math.max(...input));
    let blocks = input[maxIndex];
    input[maxIndex] = 0;
    
    while (blocks > 0) {
        maxIndex = (maxIndex + 1) % input.length;
        input[maxIndex]++;
        blocks--;
    }
    
    cycles++;
}

console.log(cycles);
console.log(cycles - [...seenConfigs.keys()].indexOf(input.join(',')));