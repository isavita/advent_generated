const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n').filter(Boolean);

for (let i = 0; i < input.length - 1; i++) {
    for (let j = i + 1; j < input.length; j++) {
        let diff = 0;
        for (let k = 0; k < input[i].length; k++) {
            if (input[i][k] !== input[j][k]) {
                diff++;
                if (diff > 1) {
                    break;
                }
            }
        }
        if (diff === 1) {
            let common = '';
            for (let k = 0; k < input[i].length; k++) {
                if (input[i][k] === input[j][k]) {
                    common += input[i][k];
                }
            }
            console.log(common);
            process.exit();
        }
    }
}