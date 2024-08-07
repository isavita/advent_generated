import * as fs from 'fs';

function main() {
    const filePath = 'input.txt';
    const data = fs.readFileSync(filePath, 'utf-8').trim();

    let floor = 0;
    let basementPosition = -1;

    for (let i = 0; i < data.length; i++) {
        if (data[i] === '(') {
            floor++;
        } else if (data[i] === ')') {
            floor--;
        }

        if (floor === -1 && basementPosition === -1) {
            basementPosition = i + 1;
        }
    }

    console.log(`Final floor: ${floor}`);
    console.log(`Position of first character that causes him to enter the basement: ${basementPosition}`);
}

main();