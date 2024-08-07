import * as fs from 'fs';

const reactPolymer = (polymer: string): string => {
    const stack: string[] = [];
    for (const unit of polymer) {
        if (stack.length && stack[stack.length - 1] !== unit && stack[stack.length - 1].toLowerCase() === unit.toLowerCase()) {
            stack.pop();
        } else {
            stack.push(unit);
        }
    }
    return stack.join('');
};

const findShortestPolymer = (polymer: string): number => {
    const unitTypes = new Set(polymer.toLowerCase());
    let shortestLength = Infinity;

    for (const unit of unitTypes) {
        const modifiedPolymer = polymer.replace(new RegExp(unit, 'gi'), '');
        const reactedPolymer = reactPolymer(modifiedPolymer);
        shortestLength = Math.min(shortestLength, reactedPolymer.length);
    }

    return shortestLength;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const fullyReactedPolymer = reactPolymer(input);
    console.log(`Length of fully reacted polymer: ${fullyReactedPolymer.length}`);

    const shortestLength = findShortestPolymer(input);
    console.log(`Length of shortest polymer after removing one type: ${shortestLength}`);
};

main();