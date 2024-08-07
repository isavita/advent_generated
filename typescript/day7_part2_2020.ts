import * as fs from 'fs';

interface BagRule {
    color: string;
    contains: Map<string, number>;
}

const parseInput = (input: string): BagRule[] => {
    const rules: BagRule[] = [];
    const lines = input.trim().split('\n');

    for (const line of lines) {
        const [_, color, contents] = line.match(/(\w+ \w+) bags contain (.+)\./) || [];
        const contains = new Map<string, number>();
        if (contents !== 'no other bags') {
            contents.split(', ').forEach(item => {
                const match = item.match(/(\d+) (\w+ \w+) bags?/);
                if (match) {
                    contains.set(match[2], parseInt(match[1]));
                }
            });
        }
        rules.push({ color, contains });
    }
    return rules;
};

const canContainShinyGold = (rules: BagRule[], color: string, memo: Set<string>): boolean => {
    if (memo.has(color)) return false;
    const rule = rules.find(r => r.color === color);
    if (!rule) return false;
    if (rule.contains.has('shiny gold')) return true;

    memo.add(color);
    for (const containedColor of rule.contains.keys()) {
        if (canContainShinyGold(rules, containedColor, memo)) {
            return true;
        }
    }
    return false;
};

const countBagsInside = (rules: BagRule[], color: string): number => {
    const rule = rules.find(r => r.color === color);
    if (!rule) return 0;

    let count = 0;
    for (const [containedColor, quantity] of rule.contains.entries()) {
        count += quantity + quantity * countBagsInside(rules, containedColor);
    }
    return count;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const rules = parseInput(input);

    const colorsThatCanContainGold = new Set<string>();
    rules.forEach(rule => {
        if (canContainShinyGold(rules, rule.color, new Set())) {
            colorsThatCanContainGold.add(rule.color);
        }
    });

    const totalColors = colorsThatCanContainGold.size;
    const totalBagsInsideGold = countBagsInside(rules, 'shiny gold');

    console.log(totalColors);
    console.log(totalBagsInsideGold);
};

main();