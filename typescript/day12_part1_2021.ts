import * as fs from 'fs';

const readInput = (filePath: string): string[][] => {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.trim().split('\n').map(line => line.split('-'));
};

const buildGraph = (edges: string[][]): Map<string, string[]> => {
    const graph = new Map<string, string[]>();
    for (const [from, to] of edges) {
        if (!graph.has(from)) graph.set(from, []);
        if (!graph.has(to)) graph.set(to, []);
        graph.get(from)!.push(to);
        graph.get(to)!.push(from);
    }
    return graph;
};

const isSmallCave = (cave: string): boolean => cave === cave.toLowerCase();

const countPaths = (graph: Map<string, string[]>, current: string, visited: Set<string>): number => {
    if (current === 'end') return 1;
    if (isSmallCave(current)) visited.add(current);
    
    let totalPaths = 0;
    for (const neighbor of graph.get(current) || []) {
        if (!visited.has(neighbor)) {
            totalPaths += countPaths(graph, neighbor, visited);
        }
    }
    
    if (isSmallCave(current)) visited.delete(current);
    return totalPaths;
};

const main = () => {
    const edges = readInput('input.txt');
    const graph = buildGraph(edges);
    const totalPaths = countPaths(graph, 'start', new Set());
    console.log(totalPaths);
};

main();