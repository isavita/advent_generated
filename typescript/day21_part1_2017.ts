import * as fs from 'fs';

type Rule = {
    input: string;
    output: string;
};

const rotate = (pattern: string[]): string[] => {
    const size = pattern.length;
    return pattern.map((_, i) => pattern.map(row => row[size - 1 - i]).join(''));
};

const flip = (pattern: string[]): string[] => {
    return pattern.map(row => row.split('').reverse().join(''));
};

const generateVariations = (pattern: string[]): string[] => {
    const variations = new Set<string>();
    let current = pattern;

    for (let i = 0; i < 4; i++) {
        current = rotate(current);
        variations.add(current.join('/'));
        variations.add(flip(current).join('/'));
    }

    return Array.from(variations);
};

const parseRules = (input: string): Rule[] => {
    return input.trim().split('\n').map(line => {
        const [lhs, rhs] = line.split(' => ');
        return { input: lhs, output: rhs };
    });
};

const applyRules = (grid: string[], rules: Map<string, string>): string[] => {
    const size = grid.length;
    const newSize = size % 2 === 0 ? size / 2 * 3 : size / 3 * 4;
    const newGrid = Array.from({ length: newSize }, () => '');

    for (let i = 0; i < size; i += (size % 2 === 0 ? 2 : 3)) {
        for (let j = 0; j < size; j += (size % 2 === 0 ? 2 : 3)) {
            const subPattern = grid.slice(i, i + (size % 2 === 0 ? 2 : 3)).map(row => row.slice(j, j + (size % 2 === 0 ? 2 : 3)));
            const key = subPattern.join('/');
            const outputPattern = rules.get(key)!;

            const outputRows = outputPattern.split('/');
            const newRowOffset = (i / (size % 2 === 0 ? 2 : 3)) * (size % 2 === 0 ? 3 : 4);
            outputRows.forEach((row, rowIndex) => {
                newGrid[newRowOffset + rowIndex] += row;
            });
        }
    }

    return newGrid;
};

const countOnPixels = (grid: string[]): number => {
    return grid.join('').split('').filter(pixel => pixel === '#').length;
};

const main = async () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const rulesArray = parseRules(input);
    const rules = new Map<string, string>();

    for (const { input, output } of rulesArray) {
        const variations = generateVariations(input.split('/'));
        variations.forEach(varPattern => rules.set(varPattern, output));
    }

    let grid = ['.#.', '..#', '###'];
    for (let iteration = 0; iteration < 5; iteration++) {
        grid = applyRules(grid, rules);
    }

    console.log(countOnPixels(grid));
};

main().catch(console.error);