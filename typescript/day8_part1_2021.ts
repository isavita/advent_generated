const fs = require('fs');
const data = fs.readFileSync('input.txt', 'utf8').split('\n');

let count = 0;
for (let i = 0; i < data.length; i++) {
    const parts = data[i].split(' | ');
    const output = parts[1];
    output.split(' ').forEach(digit => {
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