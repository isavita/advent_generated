import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8');
const commands = input.trim().split('\n');

let horizontal = 0;
let depth = 0;

commands.forEach(command => {
    const [direction, valueStr] = command.split(' ');
    const value = parseInt(valueStr);

    switch (direction) {
        case 'forward':
            horizontal += value;
            break;
        case 'down':
            depth += value;
            break;
        case 'up':
            depth -= value;
            break;
    }
});

console.log(horizontal * depth);