const fs = require('fs');

class Component {
    constructor(a, b) {
        this.a = a;
        this.b = b;
    }
}

let maxStrength = 0;

function findStrongestBridge(components, used, port, strength) {
    if (strength > maxStrength) {
        maxStrength = strength;
    }

    for (let i = 0; i < components.length; i++) {
        const c = components[i];
        if (used[i]) {
            continue;
        }

        if (c.a === port || c.b === port) {
            used[i] = true;
            const nextPort = c.a === port ? c.b : c.a;
            findStrongestBridge(components, used, nextPort, strength + c.a + c.b);
            used[i] = false;
        }
    }
}

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const components = input.map(line => {
    const [a, b] = line.split('/').map(Number);
    return new Component(a, b);
});

const used = new Array(components.length).fill(false);
findStrongestBridge(components, used, 0, 0);

console.log(maxStrength);