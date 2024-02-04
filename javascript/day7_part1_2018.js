const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const deps = {};
const allSteps = {};

input.forEach(line => {
    const [, a, b] = line.match(/Step (.) must be finished before step (.) can begin./);
    if (!deps[b]) deps[b] = [];
    deps[b].push(a);
    allSteps[a] = true;
    allSteps[b] = true;
});

const topologicalSort = (deps, allSteps) => {
    const order = [];
    let available = [];

    for (const step in allSteps) {
        if (!deps[step] || deps[step].length === 0) {
            available.push(step);
        }
    }
    available.sort();

    while (available.length > 0) {
        const next = available.shift();
        order.push(next);

        for (const step in allSteps) {
            if (deps[step] && deps[step].includes(next)) {
                deps[step] = deps[step].filter(item => item !== next);
                if (deps[step].length === 0) {
                    available.push(step);
                }
            }
        }
        available.sort();
    }
    return order.join('');
};

const order = topologicalSort(deps, allSteps);
console.log(order);