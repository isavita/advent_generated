
const fs = require('fs');
const input = fs.readFileSync('input.txt', 'utf8').trim().split(',').map(Number);

function runProgram(inputArray) {
    let i = 0;
    let output = 0;

    const getValue = (mode, param) => (mode === 0 ? inputArray[param] : param);

    while (i < inputArray.length) {
        const opcode = inputArray[i] % 100;
        const mode1 = Math.floor((inputArray[i] / 100) % 10);
        const mode2 = Math.floor((inputArray[i] / 1000) % 10);
        const mode3 = Math.floor((inputArray[i] / 10000) % 10);

        if (opcode === 1) {
            const val1 = getValue(mode1, inputArray[i + 1]);
            const val2 = getValue(mode2, inputArray[i + 2]);
            inputArray[inputArray[i + 3]] = val1 + val2;
            i += 4;
        } else if (opcode === 2) {
            const val1 = getValue(mode1, inputArray[i + 1]);
            const val2 = getValue(mode2, inputArray[i + 2]);
            inputArray[inputArray[i + 3]] = val1 * val2;
            i += 4;
        } else if (opcode === 3) {
            inputArray[inputArray[i + 1]] = 5; // System ID for thermal radiator controller
            i += 2;
        } else if (opcode === 4) {
            output = getValue(mode1, inputArray[i + 1]);
            i += 2;
        } else if (opcode === 5) {
            const val1 = getValue(mode1, inputArray[i + 1]);
            const val2 = getValue(mode2, inputArray[i + 2]);
            if (val1 !== 0) {
                i = val2;
            } else {
                i += 3;
            }
        } else if (opcode === 6) {
            const val1 = getValue(mode1, inputArray[i + 1]);
            const val2 = getValue(mode2, inputArray[i + 2]);
            if (val1 === 0) {
                i = val2;
            } else {
                i += 3;
            }
        } else if (opcode === 7) {
            const val1 = getValue(mode1, inputArray[i + 1]);
            const val2 = getValue(mode2, inputArray[i + 2]);
            inputArray[inputArray[i + 3]] = val1 < val2 ? 1 : 0;
            i += 4;
        } else if (opcode === 8) {
            const val1 = getValue(mode1, inputArray[i + 1]);
            const val2 = getValue(mode2, inputArray[i + 2]);
            inputArray[inputArray[i + 3]] = val1 === val2 ? 1 : 0;
            i += 4;
        } else if (opcode === 99) {
            break;
        } else {
            console.error('Unknown opcode');
            break;
        }
    }

    return output;
}

console.log(runProgram(input));
