import * as fs from 'fs';
import * as path from 'path';

// Function to read input from file
function readInput(filePath: string): number[] {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.split('\n').map(Number);
}

// Function to find the quantum entanglement of the first group
function findQuantumEntanglement(weights: number[]): number {
    const totalWeight = weights.reduce((sum, weight) => sum + weight, 0);
    const targetWeight = totalWeight / 3;

    if (targetWeight !== Math.floor(targetWeight)) {
        throw new Error('It is not possible to split the packages into three equal groups.');
    }

    let minGroupSize = Infinity;
    let minQuantumEntanglement = Infinity;

    const findGroups = (start: number, currentGroup: number[], currentSum: number, groupSize: number) => {
        if (currentSum === targetWeight) {
            if (groupSize < minGroupSize) {
                minGroupSize = groupSize;
                minQuantumEntanglement = currentGroup.reduce((product, weight) => product * weight, 1);
            } else if (groupSize === minGroupSize) {
                const currentQE = currentGroup.reduce((product, weight) => product * weight, 1);
                if (currentQE < minQuantumEntanglement) {
                    minQuantumEntanglement = currentQE;
                }
            }
            return;
        }
        if (currentSum > targetWeight || start >= weights.length) {
            return;
        }

        for (let i = start; i < weights.length; i++) {
            currentGroup.push(weights[i]);
            findGroups(i + 1, currentGroup, currentSum + weights[i], groupSize + 1);
            currentGroup.pop();
        }
    };

    findGroups(0, [], 0, 0);

    return minQuantumEntanglement;
}

// Main function to execute the program
function main() {
    const filePath = path.join(__dirname, 'input.txt');
    const weights = readInput(filePath);
    const quantumEntanglement = findQuantumEntanglement(weights);
    console.log(quantumEntanglement);
}

main();