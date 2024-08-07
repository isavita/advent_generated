import * as fs from 'fs';

function sumCalibrationValues(filePath: string): number {
    const data = fs.readFileSync(filePath, 'utf-8');
    const lines = data.split('\n').filter(line => line.trim() !== '');

    let totalSum = 0;

    for (const line of lines) {
        const firstDigit = line.match(/\d/)?.[0];
        const lastDigit = line.match(/\d(?=[^0-9]*$)/)?.[0];

        if (firstDigit && lastDigit) {
            totalSum += parseInt(firstDigit + lastDigit, 10);
        }
    }

    return totalSum;
}

const result = sumCalibrationValues('input.txt');
console.log(result);