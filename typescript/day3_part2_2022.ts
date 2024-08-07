import * as fs from 'fs';

const calculatePriority = (char: string): number => {
    const code = char.charCodeAt(0);
    return code >= 97 ? code - 96 : code - 64 + 26;
};

const partOne = (lines: string[]): number => {
    return lines.reduce((sum, line) => {
        const mid = line.length / 2;
        const firstCompartment = new Set(line.slice(0, mid));
        const secondCompartment = line.slice(mid);
        const commonItem = [...firstCompartment].find(item => secondCompartment.includes(item));
        return sum + (commonItem ? calculatePriority(commonItem) : 0);
    }, 0);
};

const partTwo = (lines: string[]): number => {
    let sum = 0;
    for (let i = 0; i < lines.length; i += 3) {
        const group = lines.slice(i, i + 3);
        const commonItems = group.reduce((acc, line) => {
            const currentSet = new Set(line);
            return acc.filter(item => currentSet.has(item));
        }, [...group[0]]);
        const badgeItem = commonItems[0];
        if (badgeItem) {
            sum += calculatePriority(badgeItem);
        }
    }
    return sum;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf8');
    const lines = input.split('\n').filter(Boolean);
    
    const resultPartOne = partOne(lines);
    console.log(`Part One: ${resultPartOne}`);
    
    const resultPartTwo = partTwo(lines);
    console.log(`Part Two: ${resultPartTwo}`);
};

main();