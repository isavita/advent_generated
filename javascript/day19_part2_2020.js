const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf-8').trim();
const result = solve(input);
console.log(result);

function solve(input) {
    const [graph, messages] = parseInput(input);

    fillInGraph(graph, 42);
    fillInGraph(graph, 31);

    const part42 = `(${graph[42].resolved.join('|')})`;
    const part31 = `(${graph[31].resolved.join('|')})`;

    const rule8String = `(${part42})+`;

    const makeRegexp = (num) => {
        return new RegExp(`^${rule8String}${part42}{${num}}${part31}{${num}}$`);
    };

    let matchRuleZero = 0;
    for (const m of messages) {
        for (let i = 1; i < 10; i++) {
            const pattern = makeRegexp(i);
            if (pattern.test(m)) {
                matchRuleZero++;
                break;
            }
        }
    }

    return matchRuleZero;
}

function fillInGraph(graph, entry) {
    if (graph[entry].resolved.length !== 0) {
        return [...graph[entry].resolved];
    }

    for (const option of graph[entry].options) {
        let resolved = [""];
        for (const entryPoint of option) {
            const nestedResolveVals = fillInGraph(graph, entryPoint);
            let newResolved = [];
            for (const nextPiece of nestedResolveVals) {
                for (const prev of resolved) {
                    newResolved.push(prev + nextPiece);
                }
            }
            resolved = newResolved;
        }
        graph[entry].resolved.push(...resolved);
    }

    return graph[entry].resolved;
}

function parseInput(input) {
    const parts = input.split("\n\n");

    const rules = {};
    for (const r of parts[0].split("\n")) {
        if (/[a-z]/.test(r)) {
            const [num, char] = r.match(/(\d+): "([a-z])"/).slice(1,3);
            rules[parseInt(num)] = { resolved: [char], options: [] };
        } else {
            const [key, ruleString] = r.split(": ");
            const options = ruleString.split(" | ").map(option => option.split(" ").map(toInt));
            rules[parseInt(key)] = { resolved: [], options };
        }
    }

    const messages = parts[1].split("\n");

    return [rules, messages];
}

function toInt(s) {
    const n = parseInt(s);
    if (isNaN(n)) {
        throw new Error("Invalid number");
    }
    return n;
}