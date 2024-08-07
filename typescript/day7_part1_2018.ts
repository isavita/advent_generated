import * as fs from 'fs';

function main() {
    const [deps, allSteps] = parseInput('input.txt');
    const order = topologicalSort(deps, allSteps);
    console.log(order);
}

function parseInput(filename: string): [Map<string, string[]>, Set<string>] {
    const data = fs.readFileSync(filename, 'utf-8');
    const deps = new Map<string, string[]>();
    const allSteps = new Set<string>();

    data.split('\n').forEach(line => {
        const match = line.match(/Step (\w) must be finished before step (\w) can begin./);
        if (match) {
            const [_, a, b] = match;
            if (!deps.has(b)) deps.set(b, []);
            deps.get(b)!.push(a);
            allSteps.add(a);
            allSteps.add(b);
        }
    });

    return [deps, allSteps];
}

function topologicalSort(deps: Map<string, string[]>, allSteps: Set<string>): string {
    const order: string[] = [];
    let available = Array.from(allSteps).filter(step => !deps.has(step) || deps.get(step)!.length === 0).sort();

    while (available.length > 0) {
        const next = available.shift()!;
        order.push(next);

        for (const step of allSteps) {
            if (deps.has(step) && deps.get(step)!.includes(next)) {
                deps.get(step)!.splice(deps.get(step)!.indexOf(next), 1);
                if (deps.get(step)!.length === 0) {
                    available.push(step);
                }
            }
        }
        available.sort();
    }

    return order.join('');
}

main();