const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const strs = data.trim().split(',');
const original = strs.map(Number);

for (let noun = 0; noun <= 99; noun++) {
    for (let verb = 0; verb <= 99; verb++) {
        const memory = [...original];
        memory[1] = noun;
        memory[2] = verb;
        if (execute(memory) === 19690720) {
            console.log(100 * noun + verb);
            return;
        }
    }
}

function execute(memory) {
    for (let i = 0; i < memory.length; i += 4) {
        switch (memory[i]) {
            case 1:
                memory[memory[i + 3]] = memory[memory[i + 1]] + memory[memory[i + 2]];
                break;
            case 2:
                memory[memory[i + 3]] = memory[memory[i + 1]] * memory[memory[i + 2]];
                break;
            case 99:
                return memory[0];
        }
    }
    return memory[0];
}