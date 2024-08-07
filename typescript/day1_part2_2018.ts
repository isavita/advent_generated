import * as fs from 'fs';

const inputFilePath = 'input.txt';

const readInput = (filePath: string): number[] => {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.split('\n').map(line => parseInt(line, 10)).filter(Boolean);
};

const calculateFinalFrequency = (changes: number[]): number => {
    return changes.reduce((acc, change) => acc + change, 0);
};

const findFirstDuplicateFrequency = (changes: number[]): number => {
    const seenFrequencies = new Set<number>();
    let currentFrequency = 0;
    let index = 0;

    while (true) {
        if (seenFrequencies.has(currentFrequency)) {
            return currentFrequency;
        }
        seenFrequencies.add(currentFrequency);
        currentFrequency += changes[index];
        index = (index + 1) % changes.length;
    }
};

const main = () => {
    const changes = readInput(inputFilePath);
    
    const finalFrequency = calculateFinalFrequency(changes);
    console.log(`Final Frequency: ${finalFrequency}`);

    const firstDuplicateFrequency = findFirstDuplicateFrequency(changes);
    console.log(`First Duplicate Frequency: ${firstDuplicateFrequency}`);
};

main();