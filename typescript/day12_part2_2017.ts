import * as fs from 'fs';

// Function to read input from the file
function readInput(filePath: string): string[] {
    return fs.readFileSync(filePath, 'utf-8').trim().split('\n');
}

// Function to parse the input and build the graph
function buildGraph(input: string[]): Map<number, number[]> {
    const graph = new Map<number, number[]>();

    input.forEach(line => {
        const [left, right] = line.split('<->');
        const program = parseInt(left.trim(), 10);
        const connections = right.trim().split(',').map(num => parseInt(num.trim(), 10));
        graph.set(program, connections);
    });

    return graph;
}

// Function to find the group containing program ID 0
function findGroupSize(graph: Map<number, number[]>, start: number): number {
    const visited = new Set<number>();
    const queue: number[] = [start];

    while (queue.length > 0) {
        const current = queue.shift()!;
        if (!visited.has(current)) {
            visited.add(current);
            const neighbors = graph.get(current) || [];
            neighbors.forEach(neighbor => {
                if (!visited.has(neighbor)) {
                    queue.push(neighbor);
                }
            });
        }
    }

    return visited.size;
}

// Function to find the total number of groups
function findTotalGroups(graph: Map<number, number[]>): number {
    const visited = new Set<number>();
    let groups = 0;

    graph.forEach((_, program) => {
        if (!visited.has(program)) {
            groups++;
            const queue: number[] = [program];
            while (queue.length > 0) {
                const current = queue.shift()!;
                if (!visited.has(current)) {
                    visited.add(current);
                    const neighbors = graph.get(current) || [];
                    neighbors.forEach(neighbor => {
                        if (!visited.has(neighbor)) {
                            queue.push(neighbor);
                        }
                    });
                }
            }
        }
    });

    return groups;
}

// Main function to solve the problem
function main() {
    const input = readInput('input.txt');
    const graph = buildGraph(input);

    const groupSize = findGroupSize(graph, 0);
    console.log(`Number of programs in the group that contains program ID 0: ${groupSize}`);

    const totalGroups = findTotalGroups(graph);
    console.log(`Total number of groups: ${totalGroups}`);
}

main();