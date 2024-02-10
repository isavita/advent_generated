const fs = require('fs');
const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let nice = 0;
const disallowPattern = /(ab|cd|pq|xy)/;
input.forEach(line => {
    let vowels = 0;
    for (let char of line) {
        if ('aeiou'.includes(char)) {
            vowels++;
        }
    }
    
    let hasDouble = false;
    for (let i = 0; i < line.length - 1; i++) {
        if (line[i] === line[i + 1]) {
            hasDouble = true;
            break;
        }
    }
    
    if (vowels >= 3 && !disallowPattern.test(line) && hasDouble) {
        nice++;
    }
});

console.log(nice);