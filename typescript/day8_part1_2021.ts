import * as fs from 'fs';

const countUniqueSegments = (input: string): number => {
    const lines = input.trim().split('\n');
    let count = 0;

    for (const line of lines) {
        const parts = line.split(' | ');
        const outputValues = parts[1].split(' ');

        for (const value of outputValues) {
            const length = value.length;
            if (length === 2 || length === 3 || length === 4 || length === 7) {
                count++;
            }
        }
    }

    return count;
};

const main = () => {
    fs.readFile('input.txt', 'utf8', (err, data) => {
        if (err) {
            console.error(err);
            return;
        }
        const result = countUniqueSegments(data);
        console.log(result);
    });
};

main();