import * as fs from 'fs';

interface Monkey {
    items: number[];
    operation: (old: number) => number;
    testDivisor: number;
    trueMonkey: number;
    falseMonkey: number;
    inspectedCount: number;
}

const parseInput = (data: string): Monkey[] => {
    const monkeys: Monkey[] = [];
    const lines = data.trim().split('\n');
    
    for (let i = 0; i < lines.length; i += 7) {
        const items = lines[i + 1].match(/(\d+)/g)?.map(Number) || [];
        const operationStr = lines[i + 2].match(/new = (.+)/)?.[1].trim();
        const testDivisor = Number(lines[i + 3].match(/divisible by (\d+)/)?.[1]);
        const trueMonkey = Number(lines[i + 4].match(/throw to monkey (\d+)/)?.[1]);
        const falseMonkey = Number(lines[i + 5].match(/throw to monkey (\d+)/)?.[1]);
        
        const operation = new Function('old', `return ${operationStr}`) as (old: number) => number;

        monkeys.push({ items, operation, testDivisor, trueMonkey, falseMonkey, inspectedCount: 0 });
    }
    
    return monkeys;
};

const simulateRounds = (monkeys: Monkey[], rounds: number) => {
    for (let round = 0; round < rounds; round++) {
        for (const monkey of monkeys) {
            while (monkey.items.length > 0) {
                const item = monkey.items.shift()!;
                monkey.inspectedCount++;
                
                let newWorry = monkey.operation(item);
                newWorry = Math.floor(newWorry / 3);
                
                const targetMonkey = newWorry % monkey.testDivisor === 0 ? monkey.trueMonkey : monkey.falseMonkey;
                monkeys[targetMonkey].items.push(newWorry);
            }
        }
    }
};

const calculateMonkeyBusiness = (monkeys: Monkey[]): number => {
    const inspectionCounts = monkeys.map(m => m.inspectedCount);
    inspectionCounts.sort((a, b) => b - a);
    return inspectionCounts[0] * inspectionCounts[1];
};

const main = () => {
    const data = fs.readFileSync('input.txt', 'utf-8');
    const monkeys = parseInput(data);
    simulateRounds(monkeys, 20);
    const monkeyBusinessLevel = calculateMonkeyBusiness(monkeys);
    console.log(monkeyBusinessLevel);
};

main();