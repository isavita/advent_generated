import * as fs from 'fs';

interface Node {
    x: number;
    y: number;
    size: number;
    used: number;
    avail: number;
    usePercent: number;
}

function parseInput(input: string): Node[] {
    const lines = input.trim().split('\n').slice(2);
    const nodes: Node[] = [];

    for (const line of lines) {
        const [, x, y, size, used, avail, usePercent] = line.match(
            /\/dev\/grid\/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%/
        )!.map(Number);

        nodes.push({ x, y, size, used, avail, usePercent });
    }

    return nodes;
}

function countViablePairs(nodes: Node[]): number {
    let viablePairsCount = 0;

    for (let i = 0; i < nodes.length; i++) {
        for (let j = 0; j < nodes.length; j++) {
            if (i !== j && nodes[i].used > 0 && nodes[i].used <= nodes[j].avail) {
                viablePairsCount++;
            }
        }
    }

    return viablePairsCount;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const nodes = parseInput(input);
    const viablePairsCount = countViablePairs(nodes);
    console.log(viablePairsCount);
}

main();