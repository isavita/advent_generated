
import * as fs from 'fs';

const getComboVal = (op: number, A: number, B: number, C: number): number => {
    if (op <= 3) return op;
    if (op === 4) return A;
    if (op === 5) return B;
    if (op === 6) return C;
    throw new Error("invalid combo operand");
};

const solve = () => {
    const data = fs.readFileSync('input.txt', 'utf-8');
    const lines = data.split('\n');

    let A = 0, B = 0, C = 0;
    let program: number[] = [];

    for (const line of lines) {
        const s = line.trim();
        if (!s) continue;
        if (s.startsWith("Register A:")) {
            A = parseInt(s.split(":")[1].trim());
        } else if (s.startsWith("Register B:")) {
            B = parseInt(s.split(":")[1].trim());
        } else if (s.startsWith("Register C:")) {
            C = parseInt(s.split(":")[1].trim());
        } else if (s.startsWith("Program:")) {
            program = s.split(":")[1].trim().split(",").map(Number);
        }
    }

    const outputVals: string[] = [];
    let ip = 0;
    while (ip < program.length) {
        const opcode = program[ip];
        if (ip + 1 >= program.length) break;
        const operand = program[ip + 1];

        switch (opcode) {
            case 0:
                const den = getComboVal(operand, A, B, C);
                A = den === 0 ? 0 : A >> den;
                ip += 2;
                break;
            case 1:
                B ^= operand;
                ip += 2;
                break;
            case 2:
                B = getComboVal(operand, A, B, C) % 8;
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
                outputVals.push((getComboVal(operand, A, B, C) % 8).toString());
                ip += 2;
                break;
            case 6:
                const den6 = getComboVal(operand, A, B, C);
                B = A >> den6;
                ip += 2;
                break;
            case 7:
                const den7 = getComboVal(operand, A, B, C);
                C = A >> den7;
                ip += 2;
                break;
            default:
                break;
        }
    }
    console.log(outputVals.join(","));
};

solve();
