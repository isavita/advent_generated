const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\t').map(Number);

let cycles = 0;
const seen = {};

while (true) {
    const key = input.join(',');
    
    if (seen[key]) {
        console.log(cycles);
        break;
    }
    
    seen[key] = true;
    
    let max = Math.max(...input);
    let index = input.indexOf(max);
    
    input[index] = 0;
    
    while (max > 0) {
        index = (index + 1) % input.length;
        input[index]++;
        max--;
    }
    
    cycles++;
}