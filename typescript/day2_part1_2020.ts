import * as fs from 'fs';

const countValidPasswords = (filePath: string): number => {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.trim().split('\n').reduce((count, line) => {
        const [policy, password] = line.split(': ').map(s => s.trim());
        const [range, letter] = policy.split(' ');
        const [min, max] = range.split('-').map(Number);
        const occurrences = password.split('').filter(char => char === letter).length;
        return count + (occurrences >= min && occurrences <= max ? 1 : 0);
    }, 0);
};

const validPasswordCount = countValidPasswords('input.txt');
console.log(validPasswordCount);