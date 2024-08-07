import * as fs from 'fs';

function readInput(filePath: string): number[] {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.split('\n').map(Number).filter(n => !isNaN(n));
}

function findCombinations(containers: number[], target: number): number[][] {
    const result: number[][] = [];

    function backtrack(start: number, target: number, path: number[]) {
        if (target === 0) {
            result.push([...path]);
            return;
        }
        if (target < 0) {
            return;
        }
        for (let i = start; i < containers.length; i++) {
            path.push(containers[i]);
            backtrack(i + 1, target - containers[i], path);
            path.pop();
        }
    }

    backtrack(0, target, []);
    return result;
}

function main() {
    const containers = readInput('input.txt');
    const target = 150;

    const combinations = findCombinations(containers, target);
    console.log(`Part 1: ${combinations.length}`);

    const minContainers = Math.min(...combinations.map(comb => comb.length));
    const minCombinations = combinations.filter(comb => comb.length === minContainers);
    console.log(`Part 2: ${minCombinations.length}`);
}

main();