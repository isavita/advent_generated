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
    return parseInt(s, 10);
}

const input = readAll('input.txt').split('\n');
let k = [];
let l = [];
let m = [];

for (let i = 0; i < input.length; i++) {
    const line = input[i];
    let v;
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
}

const constraints = {};
let stack = [];
for (let i = 0; i < l.length; i++) {
    switch (l[i]) {
        case 1:
            stack.push(i);
            break;
        case 26:
            const pop = stack.pop();
            constraints[pop] = [i, m[pop] + k[i]];
            break;
    }
}

const max = new Array(14).fill(0);
for (let i = 0; i < 14; i++) {
    if (!(i in constraints)) {
        continue;
    }
    let vmax = 9;
    while (vmax + constraints[i][1] > 9) {
        vmax--;
    }
    max[i] = vmax;
    max[constraints[i][0]] = vmax + constraints[i][1];
}

console.log(num(max));