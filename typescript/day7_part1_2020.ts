import * as fs from 'fs';

type BagRule = {
    color: string;
    contains: Array<{ count: number; color: string }>;
};

const parseInput = (input: string): BagRule[] => {
    const lines = input.trim().split('\n');
    const rules: BagRule[] = [];

    for (const line of lines) {
        const [_, color, contents] = line.match(/(\w+ \w+) bags contain (.+)\./) || [];
        const contains = contents.split(', ').map(content => {
            const match = content.match(/(\d+) (\w+ \w+) bags?/);
            return match ? { count: parseInt(match[1]), color: match[2] } : null;
        }).filter(Boolean) as Array<{ count: number; color: string }>;

        rules.push({ color, contains });
    }
    return rules;
};

const buildGraph = (rules: BagRule[]): Map<string, string[]> => {
    const graph = new Map<string, string[]>();
    for (const { color, contains } of rules) {
        for (const { color: containedColor } of contains) {
            if (!graph.has(containedColor)) {
                graph.set(containedColor, []);
            }
            graph.get(containedColor)!.push(color);
        }
    }
    return graph;
};

const countBagColors = (graph: Map<string, string[]>, target: string): number => {
    const visited = new Set<string>();
    const stack: string[] = [target];

    while (stack.length > 0) {
        const current = stack.pop()!;
        if (!visited.has(current)) {
            visited.add(current);
            const neighbors = graph.get(current) || [];
            stack.push(...neighbors);
        }
    }
    visited.delete(target); // Exclude the target bag itself
    return visited.size;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const rules = parseInput(input);
    const graph = buildGraph(rules);
    const result = countBagColors(graph, 'shiny gold');
    console.log(result);
};

main();