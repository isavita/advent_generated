import * as fs from 'fs';

const readInput = (filePath: string): string => {
    return fs.readFileSync(filePath, 'utf-8').trim();
};

const reactPolymer = (polymer: string): number => {
    const stack: string[] = [];
    
    for (const unit of polymer) {
        const lastUnit = stack[stack.length - 1];
        if (lastUnit && lastUnit !== unit && lastUnit.toLowerCase() === unit.toLowerCase()) {
            stack.pop();
        } else {
            stack.push(unit);
        }
    }
    
    return stack.length;
};

const main = () => {
    const polymer = readInput('input.txt');
    const result = reactPolymer(polymer);
    console.log(result);
};

main();