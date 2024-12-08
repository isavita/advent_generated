
import fs from 'fs';

interface Rule {
    x: number;
    y: number;
}

const readInput = (filename: string): { orderingRules: Rule[], updates: number[][] } => {
    const fileContent = fs.readFileSync(filename, 'utf-8');
    const lines = fileContent.trim().split('\n');
    const orderingRules: Rule[] = [];
    const updates: number[][] = [];
    let isUpdateSection = false;

    for (const line of lines) {
        const trimmedLine = line.trim();
        if (trimmedLine === '') {
            isUpdateSection = true;
            continue;
        }

        if (!isUpdateSection) {
            const parts = trimmedLine.split('|');
            if (parts.length === 2) {
                const x = parseInt(parts[0].trim());
                const y = parseInt(parts[1].trim());
                if (!isNaN(x) && !isNaN(y)) {
                    orderingRules.push({ x, y });
                }
            }
        } else {
            const nums = trimmedLine.split(',').map(numStr => parseInt(numStr.trim()));
            updates.push(nums.filter(num => !isNaN(num)));
        }
    }
    return { orderingRules, updates };
};


const isCorrectlyOrdered = (update: number[], rules: Rule[]): boolean => {
    const position = new Map<number, number>();
    for (let i = 0; i < update.length; i++) {
        position.set(update[i], i);
    }

    for (const { x, y } of rules) {
        const posX = position.get(x);
        const posY = position.get(y);
        if (posX !== undefined && posY !== undefined && posX >= posY) {
            return false;
        }
    }
    return true;
};

const topologicalSort = (graph: Map<number, number[]>): number[] => {
    const inDegree = new Map<number, number>();
    for (const [node, neighbors] of graph) {
        inDegree.set(node, 0);
    }
    for (const [node, neighbors] of graph) {
        for (const neighbor of neighbors) {
            inDegree.set(neighbor, (inDegree.get(neighbor) || 0) + 1);
        }
    }

    const queue: number[] = [];
    for (const [node, degree] of inDegree) {
        if (degree === 0) {
            queue.push(node);
        }
    }

    const sorted: number[] = [];
    while (queue.length > 0) {
        const node = queue.shift()!;
        sorted.push(node);
        for (const neighbor of graph.get(node) || []) {
            inDegree.set(neighbor, inDegree.get(neighbor)! - 1);
            if (inDegree.get(neighbor) === 0) {
                queue.push(neighbor);
            }
        }
    }
    return sorted;
};


const sortUpdate = (update: number[], rules: Rule[]): number[] => {
    const graph = new Map<number, number[]>();
    const pagesInUpdate = new Set(update);
    for (const page of update) {
        graph.set(page, []);
    }
    for (const { x, y } of rules) {
        if (pagesInUpdate.has(x) && pagesInUpdate.has(y)) {
            graph.get(x)!.push(y);
        }
    }
    const sorted = topologicalSort(graph);
    return sorted.reverse();
};


const main = () => {
    const { orderingRules, updates } = readInput('input.txt');
    let sum = 0;
    for (const update of updates) {
        if (!isCorrectlyOrdered(update, orderingRules)) {
            const sortedUpdate = sortUpdate(update, orderingRules);
            sum += sortedUpdate[Math.floor(sortedUpdate.length / 2)];
        }
    }
    console.log(sum);
};

main();
