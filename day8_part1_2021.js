const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const lines = data.split('\n');

let count = 0;
for (let i = 0; i < lines.length; i++) {
    const parts = lines[i].split(' | ');
    const output = parts[1].split(' ');
    output.forEach(digit => {
        switch (digit.length) {
            case 2:
            case 4:
            case 3:
            case 7:
                count++;
        }
    });
}

console.log(count);