
const fs = require('fs');

const OFFSET = 10000000000000;

function solveMachine(m) {
    const D = m.ax * m.by - m.ay * m.bx;
    if (D === 0) {
        return -1;
    }
    const numA = m.px * m.by - m.py * m.bx;
    const numB = -m.px * m.ay + m.py * m.ax;
    if (numA % D !== 0 || numB % D !== 0) {
        return -1;
    }
    const a = numA / D;
    const b = numB / D;
    if (a < 0 || b < 0) {
        return -1;
    }
    return 3 * a + b;
}

function parseMachine(lines) {
    const m = { ax: 0, ay: 0, bx: 0, by: 0, px: 0, py: 0 };
    for (const l of lines) {
        const line = l.replace(/Button A:/g, 'A:').replace(/Button B:/g, 'B:').replace(/Prize:/g, 'P:');
        if (line.startsWith('A:')) {
            [m.ax, m.ay] = parseLine(line.substring(2));
        } else if (line.startsWith('B:')) {
            [m.bx, m.by] = parseLine(line.substring(2));
        } else if (line.startsWith('P:')) {
            [m.px, m.py] = parsePrize(line.substring(2));
        }
    }
    return m;
}

function parseLine(s) {
    const parts = s.trim().split(',');
    return [parseVal(parts[0]), parseVal(parts[1])];
}

function parsePrize(s) {
    const parts = s.trim().split(',');
    return [parseValPrize(parts[0]), parseValPrize(parts[1])];
}

function parseVal(s) {
    s = s.trim();
    s = s.replace(/X\+/g, '').replace(/Y\+/g, '').replace(/X=/g, '').replace(/Y=/g, '');
    return parseInt(s, 10);
}

function parseValPrize(s) {
    s = s.trim().replace(/X=/g, '').replace(/Y=/g, '');
    return parseInt(s, 10);
}

function readInput(filename) {
    const data = fs.readFileSync(filename, 'utf-8');
    const lines = data.trim().split('\n');
    const machines = [];
    let currentMachineLines = [];

    for (const line of lines) {
        if (line.trim() === '') {
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

function main() {
    let machines = readInput('input.txt');
    machines = machines.map(m => ({
        ax: m.ax,
        ay: m.ay,
        bx: m.bx,
        by: m.by,
        px: m.px + OFFSET,
        py: m.py + OFFSET
    }));

    const results = [];
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
    let sum = 0;
    for (const c of results) {
        sum += c;
    }
    console.log(`${count} ${sum}`);
}

main();
