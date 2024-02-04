const fs = require('fs');

class Cave {
    constructor() {
        this.connections = new Set();
    }

    connectTo(name) {
        this.connections.add(name);
    }

    disconnectFrom(name) {
        this.connections.delete(name);
    }
}

const caves = {};

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

for (const line of input) {
    const [from, to] = line.split('-');

    if (!caves[from]) {
        caves[from] = new Cave();
    }

    if (!caves[to]) {
        caves[to] = new Cave();
    }

    caves[from].connectTo(to);
    caves[to].connectTo(from);
}

let count = 0;

const dfs = (current, visited) => {
    if (current === 'end') {
        count++;
        return;
    }

    for (const next of caves[current].connections) {
        if (visited[next] && next.toLowerCase() === next) {
            continue;
        }

        const visitedCopy = { ...visited };
        visitedCopy[next] = true;
        dfs(next, visitedCopy);
    }
};

dfs('start', { start: true });

console.log(count);