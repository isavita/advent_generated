import * as fs from 'fs';

const calculateFrequency = (changes: string[]): number => {
    return changes.reduce((total, change) => total + parseInt(change), 0);
};

const main = () => {
    const data = fs.readFileSync('input.txt', 'utf-8');
    const changes = data.split('\n').filter(line => line.trim() !== '');
    const result = calculateFrequency(changes);
    console.log(result);
};

main();