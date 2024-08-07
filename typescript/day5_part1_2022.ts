import * as fs from 'fs';

function main() {
    const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n\n');
    const stackInput = input[0].split('\n');
    const stacks: string[][] = Array.from({ length: (stackInput[0].length + 1) / 4 }, () => []);

    for (const line of stackInput) {
        for (let i = 0; i < line.length; i++) {
            if (line[i] >= 'A' && line[i] <= 'Z') {
                stacks[(i - 1) / 4].push(line[i]);
            }
        }
    }

    const steps = input[1].split('\n');
    console.log(move(stacks, steps));
}

function move(st: string[][], steps: string[]): string {
    const stacks = st.map(stack => [...stack].reverse());

    for (const step of steps) {
        const [, n, from, to] = step.match(/move (\d+) from (\d+) to (\d+)/)!.map(Number);
        for (let i = 0; i < n; i++) {
            stacks[to - 1].push(stacks[from - 1].pop()!);
        }
    }

    return stacks.map(stack => stack[stack.length - 1]).join('');
}

main();