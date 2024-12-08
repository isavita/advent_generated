
const fs = require('node:fs');

function solve() {
  const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
  let totalCalibrationResult = 0;

  for (const line of input) {
    const [testValueStr, numbersStr] = line.split(':');
    const testValue = parseInt(testValueStr);
    const numbers = numbersStr.trim().split(' ').map(Number);

    if (canBeTrue(testValue, numbers)) {
      totalCalibrationResult += testValue;
    }
  }

  console.log(totalCalibrationResult);
}


function canBeTrue(testValue, numbers) {
  const numOperators = numbers.length - 1;
  const operatorCombinations = 2 ** numOperators;

  for (let i = 0; i < operatorCombinations; i++) {
    let result = numbers[0];
    let currentCombination = i.toString(2).padStart(numOperators, '0');

    for (let j = 0; j < numOperators; j++) {
      const operator = currentCombination[j] === '0' ? '+' : '*';
      result = eval(`${result} ${operator} ${numbers[j + 1]}`);
    }

    if (result === testValue) {
      return true;
    }
  }
  return false;
}


solve();
