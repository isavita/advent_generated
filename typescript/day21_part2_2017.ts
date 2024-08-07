import * as fs from 'fs';

type Rule = {
    input: string;
    output: string;
};

const parseInput = (data: string): Rule[] => {
    return data.trim().split('\n').map(line => {
        const [input, output] = line.split(' => ');
        return { input, output };
    });
};

const getAllTransformations = (pattern: string): string[] => {
    const rotations = [pattern];
    let current = pattern;

    for (let i = 0; i < 3; i++) {
        current = current.split('/').map((_, idx, arr) => arr.map(row => row[idx]).reverse().join('')).join('/');
        rotations.push(current);
    }

    const flipped = pattern.split('/').reverse().join('/');
    rotations.push(flipped);

    current = flipped;
    for (let i = 0; i < 3; i++) {
        current = current.split('/').map((_, idx, arr) => arr.map(row => row[idx]).reverse().join('')).join('/');
        rotations.push(current);
    }

    return rotations;
};

const enhance = (grid: string[], rules: Rule[]): string[] => {
    const size = grid.length;
    const step = size % 2 === 0 ? 2 : 3;
    const newSize = (size / step) * (step + 1);
    const newGrid: string[] = Array.from({ length: newSize }, () => '');

    for (let i = 0; i < size; i += step) {
        for (let j = 0; j < size; j += step) {
            const block = grid.slice(i, i + step).map(row => row.slice(j, j + step)).join('/');
            const rule = rules.find(r => getAllTransformations(r.input).includes(block));
            if (rule) {
                const output = rule.output.split('/');
                for (let k = 0; k < output.length; k++) {
                    newGrid[i / step * (step + 1) + k] += output[k];
                }
            }
        }
    }
    return newGrid;
};

const countOnPixels = (grid: string[]): number => {
    return grid.reduce((count, row) => count + row.split('').filter(pixel => pixel === '#').length, 0);
};

const main = () => {
    const data = fs.readFileSync('input.txt', 'utf-8');
    const rules = parseInput(data);
    
    let grid = ['.#.', '..#', '###'];
    for (let i = 0; i < 18; i++) {
        grid = enhance(grid, rules);
    }
    
    const result = countOnPixels(grid);
    console.log(result);
};

main();