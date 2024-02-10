const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

const contains = {};

for (const line of input) {
    const parts = line.split(' bags contain ');
    const container = parts[0];
    if (parts[1] === 'no other bags.') {
        continue;
    }
    const containedBags = parts[1].split(', ');
    for (const bag of containedBags) {
        const bagName = bag.split(' ').slice(1, 3).join(' ');
        if (!contains[bagName]) {
            contains[bagName] = [];
        }
        contains[bagName].push(container);
    }
}

const countCanContain = (target, contains) => {
    const seen = {};
    const dfs = (bag) => {
        for (const outer of contains[bag] || []) {
            if (!seen[outer]) {
                seen[outer] = true;
                dfs(outer);
            }
        }
    };
    dfs(target);
    return Object.keys(seen).length;
};

const count = countCanContain('shiny gold', contains);
console.log(count);