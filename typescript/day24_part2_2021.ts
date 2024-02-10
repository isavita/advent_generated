
const fs = require('fs');

function readAll(path) {
    return fs.readFileSync(path, 'utf8').trim();
}

function num(w) {
    let n = 0;
    for (let i = 0; i < w.length; i++) {
        n *= 10;
        n += w[i];
    }
    return n;
}

function reg(r) {
    return r.charCodeAt(0) - 'w'.charCodeAt(0);
}

function atoi(s) {
    const n = parseInt(s, 10);
    if (isNaN(n)) {
        throw new Error('Invalid number');
    }
    return n;
}

const input = readAll("input.txt");
const k = [];
const l = [];
const m = [];

input.split('\n').forEach((line, i) => {
    let v = 0;
    switch (i % 18) {
        case 4:
            v = parseInt(line.split(' ')[2], 10);
            l.push(v);
            break;
        case 5:
            v = parseInt(line.split(' ')[2], 10);
            k.push(v);
            break;
        case 15:
            v = parseInt(line.split(' ')[2], 10);
            m.push(v);
            break;
    }
});

const constraints = {};
const stack = [];

for (let i = 0; i < l.length; i++) {
    if (l[i] === 1) {
        stack.push(i);
    } else if (l[i] === 26) {
        const pop = stack.pop();
        constraints[pop] = [i, m[pop] + k[i]];
    }
}

const min = new Array(14).fill(0);

for (let i = 0; i < 14; i++) {
    if (constraints[i] === undefined) {
        continue;
    }
    let vmin = 1;
    while (vmin + constraints[i][1] < 1) {
        vmin++;
    }
    min[i] = vmin;
    min[constraints[i][0]] = vmin + constraints[i][1];
}

console.log(num(min));
