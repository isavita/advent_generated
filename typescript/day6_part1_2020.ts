import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim();
const groups = input.split('\n\n');

const totalYesCount = groups.reduce((sum, group) => {
    const uniqueQuestions = new Set(group.replace(/\n/g, '').split(''));
    return sum + uniqueQuestions.size;
}, 0);

console.log(totalYesCount);