import * as fs from 'fs';

interface Command {
    direction: string;
    value: number;
}

const parseInput = (input: string): Command[] => {
    return input.trim().split('\n').map(line => {
        const [direction, value] = line.split(' ');
        return { direction, value: parseInt(value, 10) };
    });
};

const calculatePositions = (commands: Command[]) => {
    let horizontal = 0, depth = 0, aim = 0;

    for (const { direction, value } of commands) {
        switch (direction) {
            case 'forward':
                horizontal += value;
                depth += aim * value;
                break;
            case 'down':
                aim += value;
                break;
            case 'up':
                aim -= value;
                break;
        }
    }

    return horizontal * depth;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const commands = parseInput(input);
    const result = calculatePositions(commands);
    console.log(result);
};

main();