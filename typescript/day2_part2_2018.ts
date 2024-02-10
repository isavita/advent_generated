const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const lines = input.slice(0, input.length - 1);

for (let i = 0; i < lines.length - 1; i++) {
    for (let j = i + 1; j < lines.length; j++) {
        let diff = 0;
        for (let k = 0; k < lines[i].length; k++) {
            if (lines[i][k] !== lines[j][k]) {
                diff++;
                if (diff > 1) {
                    break;
                }
            }
        }
        if (diff === 1) {
            let common = '';
            for (let k = 0; k < lines[i].length; k++) {
                if (lines[i][k] === lines[j][k]) {
                    common += lines[i][k];
                }
            }
            console.log(common);
            process.exit();
        }
    }
}