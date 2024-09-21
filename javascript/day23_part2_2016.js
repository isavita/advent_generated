const fs = require('fs');

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').split('\n').filter(line => line.trim());

    // Part One Execution
    let registersPartOne = new Int32Array(4); // Registers: a, b, c, d
    registersPartOne[0] = 7; // Initialize 'a' to 7
    executeInstructions(input.slice(), registersPartOne);
    console.log('Part One - Value sent to the safe:', registersPartOne[0]);

    // Part Two Execution
    let registersPartTwo = new Int32Array(4); // Reset registers for Part Two
    registersPartTwo[0] = 12; // Initialize 'a' to 12
    executeInstructions(input.slice(), registersPartTwo);
    console.log('Part Two - Value sent to the safe:', registersPartTwo[0]);

    // Optionally, log final state for verification
    // console.log('Final Registers Part Two:', Array.from(registersPartTwo));
}

function executeInstructions(instructions, registers) {
    let pc = 0; // Program counter

    while (pc < instructions.length) {
        // Ensure PC is within bounds
        if (pc < 0 || pc >= instructions.length) {
            console.error(`Program counter out of bounds: ${pc}. Terminating execution.`);
            break;
        }

        const currentInstr = instructions[pc].trim();
        const parts = currentInstr.split(' ');
        const op = parts[0];
        const x = parts[1];
        const y = parts[2];

        // Enhanced Loop Optimization: Detect and optimize nested loops
        if (
            op === 'cpy' && isRegister(y) &&
            instructions[pc + 1]?.startsWith('inc') &&
            instructions[pc + 2]?.startsWith('dec') &&
            instructions[pc + 3]?.startsWith('jnz') &&
            instructions[pc + 4]?.startsWith('dec') &&
            instructions[pc + 5]?.startsWith('jnz')
        ) {
            // Parse the loop instructions
            const incInstr = instructions[pc + 1].trim().split(' ');
            const decInstr1 = instructions[pc + 2].trim().split(' ');
            const jnzInstr1 = instructions[pc + 3].trim().split(' ');
            const decInstr2 = instructions[pc + 4].trim().split(' ');
            const jnzInstr2 = instructions[pc + 5].trim().split(' ');

            // Validate the jump offsets
            if (
                jnzInstr1[0] === 'jnz' && jnzInstr1[2] === '-2' &&
                jnzInstr2[0] === 'jnz' && jnzInstr2[2] === '-5'
            ) {
                const registerC = decInstr1[1];
                const registerD = decInstr2[1];

                // Ensure that the loop decrements 'c' and 'd' correctly
                if (isRegister(registerC) && isRegister(registerD)) {
                    const valueB = getValue(x, registers); // Value to multiply

                    // Register indices
                    const indexA = getRegisterIndex('a');
                    const indexB = getRegisterIndex('b');
                    const indexC = getRegisterIndex(registerC);
                    const indexD = getRegisterIndex(registerD);

                    const valueD = registers[indexD];
                    const valueC = registers[indexC];

                    // Calculate the multiplication effect
                    registers[indexA] += valueB * valueD;
                    registers[indexC] = 0;
                    registers[indexD] = 0;

                    // Skip the optimized instructions
                    pc += 6;
                    continue;
                }
            }
        }

        switch (op) {
            case 'cpy':
                if (isRegister(y)) {
                    const value = getValue(x, registers);
                    registers[getRegisterIndex(y)] = value;
                }
                pc++;
                break;
            case 'inc':
                if (isRegister(x)) {
                    registers[getRegisterIndex(x)]++;
                }
                pc++;
                break;
            case 'dec':
                if (isRegister(x)) {
                    registers[getRegisterIndex(x)]--;
                }
                pc++;
                break;
            case 'jnz':
                const condition = getValue(x, registers);
                const jumpOffset = getValue(y, registers);
                if (condition !== 0) {
                    pc += jumpOffset;
                } else {
                    pc++;
                }
                break;
            case 'tgl':
                const target = pc + getValue(x, registers);
                if (target >= 0 && target < instructions.length) {
                    instructions[target] = toggleInstruction(instructions[target]);
                }
                pc++;
                break;
            default:
                // If the operation is unknown, skip it
                pc++;
        }
    }
}

function getRegisterIndex(reg) {
    switch (reg) {
        case 'a': return 0;
        case 'b': return 1;
        case 'c': return 2;
        case 'd': return 3;
        default:
            throw new Error(`Unknown register: ${reg}`);
    }
}

function getValue(x, registers) {
    if (isRegister(x)) {
        return registers[getRegisterIndex(x)];
    }
    return parseInt(x, 10);
}

function isRegister(x) {
    return ['a', 'b', 'c', 'd'].includes(x);
}

function toggleInstruction(instruction) {
    const parts = instruction.trim().split(' ');
    const op = parts[0];

    switch (op) {
        case 'inc': return `dec ${parts[1]}`;
        case 'dec':
        case 'tgl': return `inc ${parts[1]}`;
        case 'jnz': return `cpy ${parts[1]} ${parts[2]}`;
        case 'cpy': return `jnz ${parts[1]} ${parts[2]}`;
        default:
            return instruction; // If unknown, leave it unchanged
    }
}

main();
