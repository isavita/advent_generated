const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n').map(Number);

for (let i = 0; i < data.length; i++) {
    for (let j = i + 1; j < data.length; j++) {
        for (let k = j + 1; k < data.length; k++) {
            if (data[i] + data[j] + data[k] === 2020) {
                console.log(data[i] * data[j] * data[k]);
                process.exit();
            }
        }
    }
}