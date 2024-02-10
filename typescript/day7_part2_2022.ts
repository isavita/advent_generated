
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const root = [''];
const dirs = {};
const files = {};
let curr = [];

input.forEach(line => {
    const txt = line.split(' ');
    if (txt[0] === '$') {
        if (txt[1] === 'cd') {
            if (txt[2] === '/') {
                curr = root;
            } else if (txt[2] === '..') {
                curr = curr.slice(0, curr.length - 1);
            } else {
                curr = curr.concat(txt[2]);
            }
            dirs[curr.join('/')] = 0;
        }
    } else {
        if (txt[0] !== 'dir') {
            files[curr.concat(txt[1]).join('/')] = parseInt(txt[0], 10);
        }
    }
});

for (const f in files) {
    const path = f.split('/');
    for (let i = 1; i < path.length; i++) {
        dirs[path.slice(0, i).join('/')] += files[f];
    }
}

const sortedSizes = Object.values(dirs).sort((a, b) => a - b);

const total = 70000000;
const want = 30000000;
const available = total - dirs[''];
console.log(sortedSizes.find(s => s >= want - available));
