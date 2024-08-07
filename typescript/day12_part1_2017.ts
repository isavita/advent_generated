import * as fs from 'fs';
import * as readline from 'readline';

async function readInput(filePath: string): Promise<string[]> {
    const fileStream = fs.createReadStream(filePath);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const lines: string[] = [];
    for await (const line of rl) {
        lines.push(line);
    }

    return lines;
}

function parseConnections(lines: string[]): Map<number, number[]> {
    const connections = new Map<number, number[]>();

    for (const line of lines) {
        const [left, right] = line.split('<->');
        const program = parseInt(left.trim(), 10);
        const connectedPrograms = right.trim().split(',').map(p => parseInt(p.trim(), 10));
        connections.set(program, connectedPrograms);
    }

    return connections;
}

function findGroupSize(connections: Map<number, number[]>, startProgram: number): number {
    const visited = new Set<number>();
    const stack: number[] = [startProgram];

    while (stack.length > 0) {
        const current = stack.pop()!;
        if (!visited.has(current)) {
            visited.add(current);
            const connectedPrograms = connections.get(current) || [];
            for (const program of connectedPrograms) {
                if (!visited.has(program)) {
                    stack.push(program);
                }
            }
        }
    }

    return visited.size;
}

async function main() {
    const lines = await readInput('input.txt');
    const connections = parseConnections(lines);
    const groupSize = findGroupSize(connections, 0);
    console.log(groupSize);
}

main().catch(console.error);