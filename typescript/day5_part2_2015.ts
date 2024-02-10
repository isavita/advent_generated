const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let nice = 0;

const passesRule1 = (line) => {
    for (let i = 0; i < line.length - 2; i++) {
        const toMatch = line.slice(i, i + 2);
        for (let j = i + 2; j < line.length - 1; j++) {
            if (line.slice(j, j + 2) === toMatch) {
                return true;
            }
        }
    }
    return false;
};

input.forEach((line) => {
    const rule1 = passesRule1(line);

    let rule2 = false;
    for (let i = 0; i < line.length - 2; i++) {
        if (line[i] === line[i + 2]) {
            rule2 = true;
            break;
        }
    }
    if (rule1 && rule2) {
        nice++;
    }
});

console.log(nice);