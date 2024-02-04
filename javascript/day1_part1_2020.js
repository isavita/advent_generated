const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n').map(Number).filter(Boolean);

for (let i = 0; i < data.length - 1; i++) {
    for (let j = i + 1; j < data.length; j++) {
        if (data[i] + data[j] === 2020) {
            console.log(data[i] * data[j]);
            process.exit();
        }
    }
}