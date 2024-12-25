
const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf-8').trim();
const lines = data.split('\n');

let A = 0, B = 0, C = 0;
let program = [];

for (const line of lines) {
    const s = line.trim();
    if (!s) continue;
    if (s.startsWith('Register A:')) {
        A = parseInt(s.split(':')[1].trim());
    } else if (s.startsWith('Register B:')) {
        B = parseInt(s.split(':')[1].trim());
    } else if (s.startsWith('Register C:')) {
        C = parseInt(s.split(':')[1].trim());
    } else if (s.startsWith('Program:')) {
        program = s.split(':')[1].trim().split(',').map(n => parseInt(n.trim()));
    }
}

const getComboVal = (op) => {
    if (op <= 3) return op;
    if (op === 4) return A;
    if (op === 5) return B;
    if (op === 6) return C;
    throw new Error("invalid combo operand");
};

const outputVals = [];
let ip = 0;
while (ip < program.length) {
    const opcode = program[ip];
    if (ip + 1 >= program.length) break;
    const operand = program[ip + 1];

    switch (opcode) {
        case 0:
            const den = getComboVal(operand);
            A = den === 0 ? 0 : Math.floor(A / (1 << den));
            ip += 2;
            break;
        case 1:
            B ^= operand;
            ip += 2;
            break;
        case 2:
            B = getComboVal(operand) % 8;
            ip += 2;
            break;
        case 3:
            ip = A !== 0 ? operand : ip + 2;
            break;
        case 4:
            B ^= C;
            ip += 2;
            break;
        case 5:
            outputVals.push((getComboVal(operand) % 8).toString());
            ip += 2;
            break;
        case 6:
            B = Math.floor(A / (1 << getComboVal(operand)));
            ip += 2;
            break;
        case 7:
            C = Math.floor(A / (1 << getComboVal(operand)));
            ip += 2;
            break;
        default:
            break;
    }
}

console.log(outputVals.join(','));
