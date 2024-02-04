const fs = require('fs');

class Program {
    constructor(weight, holds) {
        this.weight = weight;
        this.holds = holds;
    }
}

function dfs(name, programs) {
    const program = programs[name];
    let totalWeight = program.weight;

    const weights = {};
    for (const child of program.holds) {
        const [weight, balanced] = dfs(child, programs);
        if (!balanced) {
            return [0, false];
        }
        totalWeight += weight;
        weights[weight] = (weights[weight] || 0) + 1;
    }

    for (const w1 in weights) {
        for (const w2 in weights) {
            if (w1 !== w2 && weights[w1] < weights[w2]) {
                let unbalancedProgram = "";
                for (const child of program.holds) {
                    const [childWeight, _] = dfs(child, programs);
                    if (childWeight == w1) {
                        unbalancedProgram = child;
                        break;
                    }
                }
                console.log(programs[unbalancedProgram].weight + (w2 - w1));
                return [0, false];
            }
        }
    }
    return [totalWeight, true];
}

const data = fs.readFileSync('input.txt', 'utf8');
const lines = data.trim().split('\n');

const programs = {};

const re = /[a-z]+|\d+/g;

for (const line of lines) {
    const matches = line.match(re);
    const name = matches[0];
    const weight = parseInt(matches[1]);

    const program = new Program(weight, matches.slice(2));
    programs[name] = program;
}

const root = "dtacyn"; // Replace this with the root found in Part One

dfs(root, programs);