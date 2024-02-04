const fs = require('fs');

function getValue(arg, registers) {
    if (!isNaN(arg)) {
        return parseInt(arg);
    }
    return registers[arg];
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const instructions = data.trim().split('\n').map(line => line.split(' '));
    const registers = {};
    let lastSound = 0;

    for (let i = 0; i < instructions.length;) {
        const instruction = instructions[i];
        const cmd = instruction[0];
        const arg1 = instruction[1];

        switch (cmd) {
            case "snd":
                lastSound = getValue(arg1, registers);
                break;
            case "set":
                registers[arg1] = getValue(instruction[2], registers);
                break;
            case "add":
                registers[arg1] += getValue(instruction[2], registers);
                break;
            case "mul":
                registers[arg1] *= getValue(instruction[2], registers);
                break;
            case "mod":
                registers[arg1] %= getValue(instruction[2], registers);
                break;
            case "rcv":
                if (getValue(arg1, registers) !== 0) {
                    console.log(lastSound);
                    return;
                }
                break;
            case "jgz":
                if (getValue(arg1, registers) > 0) {
                    i += getValue(instruction[2], registers);
                    continue;
                }
                break;
        }
        i++;
    }
});