import * as fs from 'fs';

interface Rules {
    [key: string]: string;
}

function parseInput(filePath: string): [string, Rules] {
    const input = fs.readFileSync(filePath, 'utf-8').trim().split('\n');
    const template = input[0];
    const rules: Rules = {};
    for (let i = 2; i < input.length; i++) {
        const [pair, insertion] = input[i].split(' -> ');
        rules[pair] = insertion;
    }
    return [template, rules];
}

function polymerization(template: string, rules: Rules, steps: number): Map<string, number> {
    const pairCount = new Map<string, number>();
    for (let i = 0; i < template.length - 1; i++) {
        const pair = template.substring(i, i + 2);
        pairCount.set(pair, (pairCount.get(pair) || 0) + 1);
    }

    for (let step = 0; step < steps; step++) {
        const newPairCount = new Map<string, number>();
        for (const [pair, count] of pairCount) {
            const insertion = rules[pair];
            if (insertion) {
                const newPair1 = pair[0] + insertion;
                const newPair2 = insertion + pair[1];
                newPairCount.set(newPair1, (newPairCount.get(newPair1) || 0) + count);
                newPairCount.set(newPair2, (newPairCount.get(newPair2) || 0) + count);
            } else {
                newPairCount.set(pair, (newPairCount.get(pair) || 0) + count);
            }
        }
        pairCount.clear();
        for (const [pair, count] of newPairCount) {
            pairCount.set(pair, count);
        }
    }

    const elementCount = new Map<string, number>();
    for (const [pair, count] of pairCount) {
        elementCount.set(pair[0], (elementCount.get(pair[0]) || 0) + count);
        elementCount.set(pair[1], (elementCount.get(pair[1]) || 0) + count);
    }
    for (const key of elementCount.keys()) {
        elementCount.set(key, Math.ceil((elementCount.get(key) || 0) / 2));
    }

    return elementCount;
}

function calculateDifference(elementCount: Map<string, number>): number {
    const counts = Array.from(elementCount.values());
    return Math.max(...counts) - Math.min(...counts);
}

function main() {
    const [template, rules] = parseInput('input.txt');
    const elementCount = polymerization(template, rules, 40);
    const difference = calculateDifference(elementCount);
    console.log(difference);
}

main();