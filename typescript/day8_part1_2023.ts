import * as fs from 'fs';

interface Node {
    left: string;
    right: string;
}

const parseInput = (data: string): { instructions: string, nodes: Map<string, Node> } => {
    const lines = data.trim().split('\n');
    const instructions = lines[0];
    const nodes = new Map<string, Node>();

    for (let i = 1; i < lines.length; i++) {
        const [node, connections] = lines[i].split(' = ');
        if (!connections) continue; // Skip if connections are undefined

        const trimmedConnections = connections.trim();
        const [left, right] = trimmedConnections.slice(1, -1).split(', ').map(n => n.trim());
        nodes.set(node, { left, right });
    }

    return { instructions, nodes };
};

const navigate = (instructions: string, nodes: Map<string, Node>): number => {
    let current = 'AAA';
    let steps = 0;
    let instructionIndex = 0;

    while (current !== 'ZZZ') {
        const instruction = instructions[instructionIndex % instructions.length];
        const node = nodes.get(current);

        if (!node) break;

        current = instruction === 'R' ? node.right : node.left;
        steps++;
        instructionIndex++;
    }

    return steps;
};

const main = () => {
    const data = fs.readFileSync('input.txt', 'utf-8');
    const { instructions, nodes } = parseInput(data);
    const steps = navigate(instructions, nodes);
    console.log(steps);
};

main();