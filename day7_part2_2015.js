const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

const wireToRule = {};

input.split('\n').forEach(inst => {
    const parts = inst.split(' -> ');
    wireToRule[parts[1]] = parts[0];
});

const aSignal = memoDFS(wireToRule, 'a', {});

wireToRule['b'] = aSignal.toString();
console.log(memoDFS(wireToRule, 'a', {}));

function memoDFS(graph, entry, memo) {
    if (memo[entry] !== undefined) {
        return memo[entry];
    }

    if (/^[0-9]+$/.test(entry)) {
        return parseInt(entry, 10);
    }

    const sourceRule = graph[entry];
    const parts = sourceRule.split(' ');

    let result;
    switch (true) {
        case parts.length === 1:
            result = memoDFS(graph, parts[0], memo);
            break;
        case parts[0] === 'NOT':
            const start = memoDFS(graph, parts[1], memo);
            result = 65535 ^ start;
            break;
        case parts[1] === 'AND':
            result = memoDFS(graph, parts[0], memo) & memoDFS(graph, parts[2], memo);
            break;
        case parts[1] === 'OR':
            result = memoDFS(graph, parts[0], memo) | memoDFS(graph, parts[2], memo);
            break;
        case parts[1] === 'LSHIFT':
            result = memoDFS(graph, parts[0], memo) << memoDFS(graph, parts[2], memo);
            break;
        case parts[1] === 'RSHIFT':
            result = memoDFS(graph, parts[0], memo) >> memoDFS(graph, parts[2], memo);
            break;
    }

    memo[entry] = result;
    return result;
}