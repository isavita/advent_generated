import * as fs from 'fs';

// Function to read input from file
function readInput(filePath: string): number[] {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.split('\n').map(Number);
}

// Function to calculate the quantum entanglement
function quantumEntanglement(group: number[]): number {
    return group.reduce((acc, val) => acc * val, 1);
}

// Function to find the optimal grouping for a given number of groups
function findOptimalGrouping(weights: number[], numGroups: number): number | null {
    const totalWeight = weights.reduce((a, b) => a + b, 0);
    if (totalWeight % numGroups !== 0) return null;

    const targetWeight = totalWeight / numGroups;
    let minGroupSize = Infinity;
    let minQE = Infinity;

    const findGroup = (index: number, currentWeight: number, currentGroup: number[], groups: number[][]): void => {
        if (currentWeight === targetWeight) {
            if (currentGroup.length < minGroupSize || (currentGroup.length === minGroupSize && quantumEntanglement(currentGroup) < minQE)) {
                minGroupSize = currentGroup.length;
                minQE = quantumEntanglement(currentGroup);
            }
            return;
        }

        if (currentWeight > targetWeight || index >= weights.length) return;

        findGroup(index + 1, currentWeight + weights[index], [...currentGroup, weights[index]], groups);
        findGroup(index + 1, currentWeight, currentGroup, groups);
    };

    findGroup(0, 0, [], []);

    return minQE !== Infinity ? minQE : null;
}

const weights = readInput('input.txt');

// Part 1: Three groups
const part1Result = findOptimalGrouping(weights, 3);
console.log(`Part 1: ${part1Result}`);

// Part 2: Four groups
const part2Result = findOptimalGrouping(weights, 4);
console.log(`Part 2: ${part2Result}`);