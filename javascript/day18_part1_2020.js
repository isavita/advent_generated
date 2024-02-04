const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n').filter(Boolean);

let sum = 0;

input.forEach(expression => {
    const result = evaluate(expression);
    sum += result;
});

console.log(sum);

function evaluate(expression) {
    const tokens = tokenize(expression);
    return evaluateTokens(tokens);
}

function tokenize(expression) {
    expression = expression.replace(/\(/g, '( ').replace(/\)/g, ' )');
    return expression.split(' ').filter(Boolean);
}

function evaluateTokens(tokens) {
    const ops = [];
    const vals = [];

    tokens.forEach(token => {
        switch (token) {
            case '(':
                ops.push(token);
                break;
            case '+':
            case '*':
                while (ops.length > 0 && ops[ops.length - 1] !== '(') {
                    vals.splice(-2, 2, applyOp(ops[ops.length - 1], vals[vals.length - 2], vals[vals.length - 1]));
                    ops.pop();
                }
                ops.push(token);
                break;
            case ')':
                while (ops[ops.length - 1] !== '(') {
                    vals.splice(-2, 2, applyOp(ops[ops.length - 1], vals[vals.length - 2], vals[vals.length - 1]));
                    ops.pop();
                }
                ops.pop(); // Remove the opening '('
                break;
            default:
                const value = parseInt(token, 10);
                vals.push(value);
        }
    });

    while (ops.length > 0) {
        vals.splice(-2, 2, applyOp(ops[ops.length - 1], vals[vals.length - 2], vals[vals.length - 1]));
        ops.pop();
    }

    return vals[0];
}

function applyOp(op, a, b) {
    switch (op) {
        case '+':
            return a + b;
        case '*':
            return a * b;
        default:
            throw new Error(`Unknown operator: ${op}`);
    }
}