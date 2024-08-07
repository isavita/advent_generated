import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim();
const groups = input.split('\n\n');

const countYesAnswers = (groups: string[], all: boolean): number => {
    return groups.reduce((total, group) => {
        const answers = group.split('\n').map(person => new Set(person.split('')));
        const combinedAnswers = all ? answers.reduce((acc, curr) => {
            curr.forEach(answer => acc.add(answer));
            return acc;
        }, new Set<string>()) : answers.reduce((acc, curr) => {
            curr.forEach(answer => acc.add(answer));
            return acc;
        }, new Set<string>());
        
        return total + (all ? [...combinedAnswers].filter(answer => answers.every(set => set.has(answer))).length : combinedAnswers.size);
    }, 0);
};

const partOneResult = countYesAnswers(groups, false);
const partTwoResult = countYesAnswers(groups, true);

console.log(`Part One: ${partOneResult}`);
console.log(`Part Two: ${partTwoResult}`);