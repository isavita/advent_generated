const fs = require('fs');

const inputData = fs.readFileSync('input.txt', 'utf8').split(',').map(Number);

inputData[1] = 12;
inputData[2] = 2;

const result = executeProgram(inputData);

console.log(result);

function executeProgram(data) {
    for (let i = 0; i < data.length - 3; i += 4) {
        const pos1 = data[i + 1];
        const pos2 = data[i + 2];
        const pos3 = data[i + 3];
        switch (data[i]) {
            case 1:
                const sum = data[pos1] + data[pos2];
                data[pos3] = sum;
                break;
            case 2:
                const product = data[pos1] * data[pos2];
                data[pos3] = product;
                break;
            case 99:
                return data[0];
            default:
                throw new Error("Invalid opcode");
        }
    }

    return data[0];
}