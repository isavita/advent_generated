
const fs = require('fs');

function solve() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const machines = parseInput(input);
    let results = [];
    for (const m of machines) {
        const cost = solveMachine(m);
        if (cost >= 0) {
            results.push(cost);
        }
    }
    if (results.length === 0) {
        console.log('0 0');
        return;
    }
    const count = results.length;
    const sum = results.reduce((a, b) => a + b, 0);
    console.log(`${count} ${sum}`);
}

function parseInput(input) {
    const lines = input.split('\n').map(line => line.trim());
    const machines = [];
    let currentMachineLines = [];
    for (const line of lines) {
        if (line === '') {
            if (currentMachineLines.length > 0) {
                machines.push(parseMachine(currentMachineLines));
                currentMachineLines = [];
            }
        } else {
            currentMachineLines.push(line);
        }
    }
    if (currentMachineLines.length > 0) {
        machines.push(parseMachine(currentMachineLines));
    }
    return machines;
}

function parseMachine(lines) {
    let ax, ay, bx, by, px, py;
    for (let line of lines) {
        line = line.replace('Button A:', 'A:').replace('Button B:', 'B:').replace('Prize:', 'P:');
        if (line.startsWith('A:')) {
            [ax, ay] = parseLine(line.substring(2));
        } else if (line.startsWith('B:')) {
            [bx, by] = parseLine(line.substring(2));
        } else if (line.startsWith('P:')) {
            [px, py] = parsePrize(line.substring(2));
        }
    }
    return { ax, ay, bx, by, px, py };
}

function parseLine(s) {
    const [xp, yp] = s.split(',').map(part => part.trim());
    return [parseVal(xp), parseVal(yp)];
}

function parsePrize(s) {
    const [xp, yp] = s.split(',').map(part => part.trim());
    return [parseValPrize(xp), parseValPrize(yp)];
}

function parseVal(s) {
    s = s.trim().replace('X+', '').replace('Y+', '').replace('X=', '').replace('Y=', '');
    return parseInt(s, 10);
}

function parseValPrize(s) {
    s = s.trim().replace('X=', '').replace('Y=', '');
    return parseInt(s, 10);
}

function solveMachine(m) {
    let minCost = -1;
    const maxPresses = 100;
    for (let aCount = 0; aCount <= maxPresses; aCount++) {
        for (let bCount = 0; bCount <= maxPresses; bCount++) {
            const x = m.ax * aCount + m.bx * bCount;
            const y = m.ay * aCount + m.by * bCount;
            if (x === m.px && y === m.py) {
                const cost = aCount * 3 + bCount;
                if (minCost < 0 || cost < minCost) {
                    minCost = cost;
                }
            }
        }
    }
    return minCost;
}

solve();
