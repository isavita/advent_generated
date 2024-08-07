import * as fs from 'fs';

const evaluate = (expr: string): number => {
    const tokens = expr.match(/\d+|[\+\*\(\)]/g) || [];
    const output: string[] = [];
    const operators: string[] = [];

    const precedence = (op: string): number => (op === '+' || op === '*') ? 1 : 0;

    const applyOperator = (op: string, b: number, a: number): number => {
        return op === '+' ? a + b : a * b;
    };

    const evaluateStack = () => {
        const b = Number(output.pop());
        const a = Number(output.pop());
        const op = operators.pop()!;
        output.push(applyOperator(op, b, a).toString());
    };

    for (const token of tokens) {
        if (!isNaN(Number(token))) {
            output.push(token);
        } else if (token === '(') {
            operators.push(token);
        } else if (token === ')') {
            while (operators.length && operators[operators.length - 1] !== '(') {
                evaluateStack();
            }
            operators.pop(); // Remove '('
        } else {
            while (operators.length && precedence(operators[operators.length - 1]) >= precedence(token)) {
                evaluateStack();
            }
            operators.push(token);
        }
    }

    while (operators.length) {
        evaluateStack();
    }

    return Number(output[0]);
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const total = input.reduce((sum, line) => sum + evaluate(line), 0);
    console.log(total);
};

main();