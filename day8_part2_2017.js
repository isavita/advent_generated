const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();
const instructions = input.split('\n');

const registers = {};
let highestValue = 0;

instructions.forEach(instruction => {
    const [targetReg, op, val, _, conditionReg, conditionOp, conditionVal] = instruction.split(' ');
    
    if (!registers[targetReg]) {
        registers[targetReg] = 0;
    }
    if (!registers[conditionReg]) {
        registers[conditionReg] = 0;
    }
    
    let condition = false;
    
    switch (conditionOp) {
        case '>':
            condition = registers[conditionReg] > parseInt(conditionVal);
            break;
        case '<':
            condition = registers[conditionReg] < parseInt(conditionVal);
            break;
        case '>=':
            condition = registers[conditionReg] >= parseInt(conditionVal);
            break;
        case '<=':
            condition = registers[conditionReg] <= parseInt(conditionVal);
            break;
        case '==':
            condition = registers[conditionReg] === parseInt(conditionVal);
            break;
        case '!=':
            condition = registers[conditionReg] !== parseInt(conditionVal);
            break;
        default:
            break;
    }
    
    if (condition) {
        if (op === 'inc') {
            registers[targetReg] += parseInt(val);
        } else {
            registers[targetReg] -= parseInt(val);
        }
        
        if (registers[targetReg] > highestValue) {
            highestValue = registers[targetReg];
        }
    }
});

let maxRegisterValue = Math.max(...Object.values(registers));

console.log(maxRegisterValue);
console.log(highestValue);