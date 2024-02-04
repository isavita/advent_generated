const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n');

let sum = 0;

data.forEach(line => {
    if (line === '') {
        return;
    }

    let firstDigit = -1;
    let lastDigit = -1;

    for (let i = 0; i < line.length; i++) {
        if (!isNaN(parseInt(line[i]))) {
            if (firstDigit === -1) {
                firstDigit = parseInt(line[i]);
            }
            lastDigit = parseInt(line[i]);
        }
    }

    if (firstDigit !== -1 && lastDigit !== -1) {
        const value = parseInt(`${firstDigit}${lastDigit}`);
        sum += value;
    }
});

console.log(sum);