import * as fs from 'fs';

const hash = (input: string): number => {
    let value = 0;
    for (const char of input) {
        value = (value + char.charCodeAt(0)) * 17 % 256;
    }
    return value;
};

const processInitializationSequence = (input: string): number => {
    const steps = input.split(',').map(step => step.trim());
    return steps.reduce((sum, step) => sum + hash(step), 0);
};

const performHashMap = (input: string): number => {
    const boxes: Array<Array<{ label: string, focalLength: number }>> = Array.from({ length: 256 }, () => []);
    const steps = input.split(',').map(step => step.trim());

    for (const step of steps) {
        const match = step.match(/^(\w+)([=-])(\d*)$/);
        if (match) {
            const [_, label, operation, focalLength] = match;
            const boxIndex = hash(label);
            if (operation === '=') {
                const existingIndex = boxes[boxIndex].findIndex(lens => lens.label === label);
                if (existingIndex !== -1) {
                    boxes[boxIndex][existingIndex].focalLength = Number(focalLength);
                } else {
                    boxes[boxIndex].push({ label, focalLength: Number(focalLength) });
                }
            } else if (operation === '-') {
                const index = boxes[boxIndex].findIndex(lens => lens.label === label);
                if (index !== -1) {
                    boxes[boxIndex].splice(index, 1);
                }
            }
        }
    }

    let totalFocusingPower = 0;
    boxes.forEach((box, boxIndex) => {
        box.forEach((lens, slotIndex) => {
            totalFocusingPower += (boxIndex + 1) * (slotIndex + 1) * lens.focalLength;
        });
    });
    
    return totalFocusingPower;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').replace(/\n/g, '');
    const partOneResult = processInitializationSequence(input);
    console.log('Part One Result:', partOneResult);
    
    const partTwoResult = performHashMap(input);
    console.log('Part Two Result:', partTwoResult);
};

main();