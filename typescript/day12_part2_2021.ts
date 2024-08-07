import * as fs from 'fs';

type Graph = Record<string, string[]>;

const buildGraph = (edges: string[]): Graph => {
    const graph: Graph = {};
    for (const edge of edges) {
        const [from, to] = edge.split('-');
        if (!graph[from]) graph[from] = [];
        if (!graph[to]) graph[to] = [];
        graph[from].push(to);
        graph[to].push(from);
    }
    return graph;
};

const isSmallCave = (cave: string): boolean => cave === cave.toLowerCase();

const countPaths = (graph: Graph, current: string, visited: Set<string>, smallCaveTwice: string | null): number => {
    if (current === 'end') return 1;
    if (visited.has(current) && isSmallCave(current)) {
        if (smallCaveTwice !== null || current === 'start') return 0;
        smallCaveTwice = current; // Allow visiting this small cave for the first time
    }

    visited.add(current);
    let totalPaths = 0;

    for (const neighbor of graph[current]) {
        totalPaths += countPaths(graph, neighbor, new Set(visited), smallCaveTwice); // Pass a copy of visited
    }

    visited.delete(current);
    return totalPaths;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const edges = input.trim().split('\n');
    const graph = buildGraph(edges);
    const totalPaths = countPaths(graph, 'start', new Set(), null);
    console.log(totalPaths);
};

main();