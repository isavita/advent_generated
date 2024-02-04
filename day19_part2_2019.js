const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split(',').map(Number);

const intcodeComputer = (program, input) => {
    let memory = program.slice();
    let inputIndex = 0;
    let output = 0;
    let relativeBase = 0;
    let i = 0;

    const getMemory = (index) => {
        if (index < 0) {
            console.log('Error: Negative index');
            process.exit();
        }
        return memory[index] || 0;
    };

    const getAddress = (index, mode) => {
        if (mode === 0) {
            return getMemory(i + index);
        } else if (mode === 1) {
            return i + index;
        } else if (mode === 2) {
            return relativeBase + getMemory(i + index);
        }
    };

    const getValue = (index, mode) => {
        return getMemory(getAddress(index, mode));
    };

    const setValue = (index, value, mode) => {
        memory[getAddress(index, mode)] = value;
    };

    while (i < memory.length) {
        const opcode = getMemory(i) % 100;
        const mode1 = Math.floor(getMemory(i) / 100) % 10;
        const mode2 = Math.floor(getMemory(i) / 1000) % 10;
        const mode3 = Math.floor(getMemory(i) / 10000) % 10;

        if (opcode === 1) {
            const sum = getValue(1, mode1) + getValue(2, mode2);
            setValue(3, sum, mode3);
            i += 4;
        } else if (opcode === 2) {
            const product = getValue(1, mode1) * getValue(2, mode2);
            setValue(3, product, mode3);
            i += 4;
        } else if (opcode === 3) {
            setValue(1, input[inputIndex], mode1);
            inputIndex++;
            i += 2;
        } else if (opcode === 4) {
            output = getValue(1, mode1);
            i += 2;
        } else if (opcode === 5) {
            if (getValue(1, mode1) !== 0) {
                i = getValue(2, mode2);
            } else {
                i += 3;
            }
        } else if (opcode === 6) {
            if (getValue(1, mode1) === 0) {
                i = getValue(2, mode2);
            } else {
                i += 3;
            }
        } else if (opcode === 7) {
            const lessThan = getValue(1, mode1) < getValue(2, mode2) ? 1 : 0;
            setValue(3, lessThan, mode3);
            i += 4;
        } else if (opcode === 8) {
            const equals = getValue(1, mode1) === getValue(2, mode2) ? 1 : 0;
            setValue(3, equals, mode3);
            i += 4;
        } else if (opcode === 9) {
            relativeBase += getValue(1, mode1);
            i += 2;
        } else if (opcode === 99) {
            break;
        } else {
            console.log('Error: Invalid opcode');
            process.exit();
        }
    }

    return output;
};

const checkBeam = (x, y) => {
    return intcodeComputer(input, [x, y]);
};

const part1 = () => {
    let count = 0;
    for (let y = 0; y < 50; y++) {
        for (let x = 0; x < 50; x++) {
            count += checkBeam(x, y);
        }
    }
    return count;
};

const part2 = () => {
    let x = 0;
    let y = 99;
    while (true) {
        while (checkBeam(x, y) === 0) {
            x++;
        }
        if (checkBeam(x + 99, y - 99) === 1) {
            return x * 10000 + y - 99;
        }
        y++;
    }
};

console.log(part1());
console.log(part2());