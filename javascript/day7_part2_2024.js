
const fs = require('node:fs');

function solve() {
    const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
    let totalCalibrationResult = 0;

    for (const line of input) {
        const [testValueStr, numbersStr] = line.split(':');
        const testValue = parseInt(testValueStr);
        const numbers = numbersStr.trim().split(' ').map(Number);

        if (canBeMadeTrue(testValue, numbers)) {
            totalCalibrationResult += testValue;
        }
    }

    console.log(totalCalibrationResult);
}


function canBeMadeTrue(testValue, numbers) {
    const numOperators = numbers.length - 1;
    const operatorCombinations = 3 ** numOperators; // 3 operators: +, *, ||

    for (let i = 0; i < operatorCombinations; i++) {
        let currentCombination = i;
        let result = numbers[0];
        for (let j = 0; j < numOperators; j++) {
            const operator = Math.floor(currentCombination / (3 ** (numOperators - 1 - j))) % 3;
            const nextNumber = numbers[j + 1];

            switch (operator) {
                case 0: // +
                    result += nextNumber;
                    break;
                case 1: // *
                    result *= nextNumber;
                    break;
                case 2: // ||
                    result = parseInt(`${result}${nextNumber}`);
                    break;
            }
        }
        if (result === testValue) return true;
    }
    return false;
}


solve();
