import * as fs from 'fs';

interface Monkey {
    name: string;
    value?: number;
    operation?: [string, string, string]; // [left, operator, right]
}

const parseInput = (data: string): Map<string, Monkey> => {
    const monkeys = new Map<string, Monkey>();
    
    data.trim().split('\n').forEach(line => {
        const [name, job] = line.split(': ').map(s => s.trim());
        if (!isNaN(Number(job))) {
            monkeys.set(name, { name, value: Number(job) });
        } else {
            const [left, operator, right] = job.split(' ');
            monkeys.set(name, { name, operation: [left, operator, right] });
        }
    });
    
    return monkeys;
};

const evaluateMonkey = (monkeys: Map<string, Monkey>, name: string): number => {
    const monkey = monkeys.get(name)!;
    if (monkey.value !== undefined) {
        return monkey.value;
    }
    const [left, operator, right] = monkey.operation!;
    const leftValue = evaluateMonkey(monkeys, left);
    const rightValue = evaluateMonkey(monkeys, right);
    
    switch (operator) {
        case '+': return leftValue + rightValue;
        case '-': return leftValue - rightValue;
        case '*': return leftValue * rightValue;
        case '/': return leftValue / rightValue;
        default: throw new Error(`Unknown operator: ${operator}`);
    }
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const monkeys = parseInput(input);
    const result = evaluateMonkey(monkeys, 'root');
    console.log(result);
};

main();