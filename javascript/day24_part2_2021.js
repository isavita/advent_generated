const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let k = [];
let l = [];
let m = [];

for (let i = 0; i < input.length; i++) {
    const line = input[i];
    let v;
    switch (i % 18) {
        case 4:
            v = parseInt(line.split(' ')[2]);
            l.push(v);
            break;
        case 5:
            v = parseInt(line.split(' ')[2]);
            k.push(v);
            break;
        case 15:
            v = parseInt(line.split(' ')[2]);
            m.push(v);
            break;
    }
}

let constraints = {};
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

let min = new Array(14).fill(0);

for (let i = 0; i < 14; i++) {
    if (!constraints[i]) {
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

function num(w) {
    let n = 0;
    for (let i = 0; i < w.length; i++) {
        n *= 10;
        n += w[i];
    }
    return n;
}