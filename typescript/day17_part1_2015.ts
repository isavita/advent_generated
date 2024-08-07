import * as fs from 'fs';
import * as path from 'path';

// Read the input from the file
const inputFilePath = path.join(__dirname, 'input.txt');
const input = fs.readFileSync(inputFilePath, 'utf-8');

// Parse the input into an array of numbers
const containerSizes = input.split('\n').map(Number);

// Function to find combinations that sum to the target
function findCombinations(containers: number[], target: number, startIndex: number): number[][] {
    const combinations: number[][] = [];

    function backtrack(currentCombination: number[], remainingSum: number, currentIndex: number) {
        if (remainingSum === 0) {
            combinations.push([...currentCombination]);
            return;
        }

        if (remainingSum < 0) {
            return;
        }

        for (let i = currentIndex; i < containers.length; i++) {
            currentCombination.push(containers[i]);
            backtrack(currentCombination, remainingSum - containers[i], i + 1);
            currentCombination.pop();
        }
    }

    backtrack([], target, startIndex);
    return combinations;
}

// Find all combinations that sum to 150 liters
const combinations = findCombinations(containerSizes, 150, 0);

// Print the number of combinations
console.log(combinations.length);