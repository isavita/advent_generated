import * as fs from 'fs';

interface Monkey {
    items: number[];
    operation: (old: number) => number;
    test: number;
    trueMonkey: number;
    falseMonkey: number;
    inspectedCount: number;
}

function parseInput(input: string): Monkey[] {
    const monkeys: Monkey[] = [];
    const lines = input.trim().split('\n\n');

    for (const block of lines) {
        const lines = block.split('\n');
        const items = lines[1].match(/\d+/g)!.map(Number);
        const operationLine = lines[2].split('=')[1].trim();
        const operation = new Function('old', `return ${operationLine}`) as (old: number) => number;
        const test = Number(lines[3].match(/\d+/)![0]);
        const trueMonkey = Number(lines[4].match(/\d+/)![0]);
        const falseMonkey = Number(lines[5].match(/\d+/)![0]);

        monkeys.push({ items, operation, test, trueMonkey, falseMonkey, inspectedCount: 0 });
    }

    return monkeys;
}

function simulateMonkeys(monkeys: Monkey[], rounds: number) {
    const modulo = monkeys.reduce((acc, monkey) => acc * monkey.test, 1);

    for (let round = 0; round < rounds; round++) {
        for (const monkey of monkeys) {
            while (monkey.items.length > 0) {
                const item = monkey.items.shift()!;
                monkey.inspectedCount++;
                const newWorryLevel = monkey.operation(item) % modulo;

                if (newWorryLevel % monkey.test === 0) {
                    monkeys[monkey.trueMonkey].items.push(newWorryLevel);
                } else {
                    monkeys[monkey.falseMonkey].items.push(newWorryLevel);
                }
            }
        }
    }
}

function calculateMonkeyBusiness(monkeys: Monkey[]): number {
    const inspectionCounts = monkeys.map(m => m.inspectedCount);
    inspectionCounts.sort((a, b) => b - a);
    return inspectionCounts[0] * inspectionCounts[1];
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const monkeys = parseInput(input);
    simulateMonkeys(monkeys, 10000);
    const monkeyBusinessLevel = calculateMonkeyBusiness(monkeys);
    console.log(monkeyBusinessLevel);
}

main();