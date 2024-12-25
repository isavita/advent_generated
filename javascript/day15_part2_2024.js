
const fs = require('fs');

const Up = { x: 0, y: -1 };
const Down = { x: 0, y: 1 };
const Left = { x: -1, y: 0 };
const Right = { x: 1, y: 0 };

function solve(input) {
    let { m, steps } = parse(input);
    let robot = null;
    for (const [k, v] of Object.entries(m)) {
        if (v === '@') {
            robot = k.split(',').map(Number);
            break;
        }
    }

    for (const dir of steps) {
        if (tryToStep(m, robot, dir)) {
            robot[0] += dir.x;
            robot[1] += dir.y;
        }
    }

    let sum = 0;
    for (const [k, v] of Object.entries(m)) {
        if (v === '[' || v === 'O') {
            const [x, y] = k.split(',').map(Number);
            sum += x + 100 * y;
        }
    }
    return sum;
}

function tryToStep(m, pos, dir) {
    const orig = copyMap(m);
    const key = pos.join(',');
    if (m[key] === '.') {
        return true;
    } else if (m[key] === 'O' || m[key] === '@') {
        const nextPos = [pos[0] + dir.x, pos[1] + dir.y];
        if (tryToStep(m, nextPos, dir)) {
            m[nextPos.join(',')] = m[key];
            m[key] = '.';
            return true;
        }
    } else if (m[key] === ']') {
        const nextPos = [pos[0] + Left.x, pos[1] + Left.y];
        if (tryToStep(m, nextPos, dir)) {
            return true;
        }
    } else if (m[key] === '[') {
        if (dir === Left) {
            const nextPos = [pos[0] + Left.x, pos[1] + Left.y];
            if (tryToStep(m, nextPos, dir)) {
                m[nextPos.join(',')] = '[';
                m[key] = ']';
                m[[pos[0] + Right.x, pos[1] + Right.y].join(',')] = '.';
                return true;
            }
        } else if (dir === Right) {
            const nextPos = [pos[0] + 2 * Right.x, pos[1] + 2 * Right.y];
            if (tryToStep(m, nextPos, dir)) {
                m[key] = '.';
                m[[pos[0] + Right.x, pos[1] + Right.y].join(',')] = '[';
                m[nextPos.join(',')] = ']';
                return true;
            }
        } else {
            const nextPos = [pos[0] + dir.x, pos[1] + dir.y];
            const nextPosRight = [pos[0] + Right.x + dir.x, pos[1] + Right.y + dir.y];
            if (tryToStep(m, nextPos, dir) && tryToStep(m, nextPosRight, dir)) {
                m[key] = '.';
                m[[pos[0] + Right.x, pos[1] + Right.y].join(',')] = '.';
                m[nextPos.join(',')] = '[';
                m[nextPosRight.join(',')] = ']';
                return true;
            }
        }
    }
    
    for (const k in m) {
        if (orig.hasOwnProperty(k)) {
            m[k] = orig[k];
        } else {
            delete m[k];
        }
    }
    return false;
}

function scaleUp(input) {
    let s = input;
    s = s.replace(/#/g, "##");
    s = s.replace(/\./g, "..");
    s = s.replace(/O/g, "[]");
    s = s.replace(/@/g, "@.");
    return s;
}

function parse(input) {
    const blocks = input.trim().split("\n\n");
    const lines = blocks[0].split("\n");
    const m = {};
    for (let y = 0; y < lines.length; y++) {
        for (let x = 0; x < lines[y].length; x++) {
            m[[x, y]] = lines[y][x];
        }
    }
    const steps = [];
    for (const ch of blocks[1].replace(/\n/g, "")) {
        switch (ch) {
            case '^':
                steps.push(Up);
                break;
            case '<':
                steps.push(Left);
                break;
            case '>':
                steps.push(Right);
                break;
            case 'v':
                steps.push(Down);
                break;
        }
    }
    return { m, steps };
}

function copyMap(src) {
    const dst = {};
    for (const k in src) {
        dst[k] = src[k];
    }
    return dst;
}

const input = fs.readFileSync("input.txt", "utf-8");
console.log(Math.round(solve(input)));
console.log(Math.round(solve(scaleUp(input))));
