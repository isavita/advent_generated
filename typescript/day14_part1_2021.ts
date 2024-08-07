import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
const template = input[0];
const rules = new Map<string, string>();

for (let i = 2; i < input.length; i++) {
    const [pair, insertion] = input[i].split(' -> ');
    rules.set(pair, insertion);
}

const polymerize = (template: string, rules: Map<string, string>, steps: number): string => {
    let current = template;

    for (let step = 0; step < steps; step++) {
        let next = '';
        for (let i = 0; i < current.length - 1; i++) {
            const pair = current.substring(i, i + 2);
            next += current[i] + (rules.get(pair) || '');
        }
        next += current[current.length - 1]; // Add last character
        current = next;
    }

    return current;
};

const countElements = (polymer: string): Map<string, number> => {
    const counts = new Map<string, number>();
    for (const char of polymer) {
        counts.set(char, (counts.get(char) || 0) + 1);
    }
    return counts;
};

const polymer = polymerize(template, rules, 10);
const counts = countElements(polymer);

const maxCount = Math.max(...Array.from(counts.values()));
const minCount = Math.min(...Array.from(counts.values()));

console.log(maxCount - minCount);