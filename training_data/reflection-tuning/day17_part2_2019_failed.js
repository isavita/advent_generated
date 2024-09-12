const IntcodeComputer = require('./intcodeComputer'); // Assuming we have this implementation

function parseScaffold(output) {
    return output.trim().split('\n').map(line => line.split(''));
}

function findIntersections(grid) {
    let sum = 0;
    for (let y = 1; y < grid.length - 1; y++) {
        for (let x = 1; x < grid[y].length - 1; x++) {
            if (grid[y][x] === '#' &&
                grid[y-1][x] === '#' &&
                grid[y+1][x] === '#' &&
                grid[y][x-1] === '#' &&
                grid[y][x+1] === '#') {
                sum += x * y;
            }
        }
    }
    return sum;
}

function solvePart1(program) {
    const computer = new IntcodeComputer(program);
    const output = computer.run().map(code => String.fromCharCode(code)).join('');
    const grid = parseScaffold(output);
    return findIntersections(grid);
}

function solvePart2(program) {
    program[0] = 2; // Wake up the robot
    const computer = new IntcodeComputer(program);
    
    const mainRoutine = 'A,B,A,C,A,B,C,B,C,B\n';
    const functionA = 'L,10,R,8,R,6,R,10\n';
    const functionB = 'L,12,R,8,L,12\n';
    const functionC = 'L,10,R,6,L,4\n';
    const videoFeed = 'n\n';

    const input = mainRoutine + functionA + functionB + functionC + videoFeed;
    
    let result = 0;
    computer.setInputFunction(() => {
        if (input.length > 0) {
            return input.shift().charCodeAt(0);
        }
    });

    computer.setOutputFunction((value) => {
        if (value > 255) {
            result = value;
        }
    });

    computer.run();
    return result;
}

// Assuming we have the puzzle input as an array of numbers called 'input'
console.log('Part 1:', solvePart1(input));
console.log('Part 2:', solvePart2(input));
