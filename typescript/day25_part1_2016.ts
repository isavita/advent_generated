import * as fs from 'fs';

function main() {
    const instructions = fs.readFileSync('input.txt', 'utf-8').split('\n');

    for (let a = 1; ; a++) {
        if (producesClockSignal(a, instructions)) {
            console.log(a);
            break;
        }
    }
}

function producesClockSignal(a: number, instructions: string[]): boolean {
    const registers: { [key: string]: number } = { a, b: 0, c: 0, d: 0 };
    let lastOutput = 0, outputCount = 0;

    for (let i = 0; i < instructions.length; ) {
        const parts = instructions[i].split(' ');
        switch (parts[0]) {
            case 'cpy':
                registers[parts[2]] = getValue(parts[1], registers);
                break;
            case 'inc':
                registers[parts[1]]++;
                break;
            case 'dec':
                registers[parts[1]]--;
                break;
            case 'jnz':
                const val1 = getValue(parts[1], registers);
                if (val1 !== 0) {
                    i += parseInt(parts[2]);
                    continue;
                }
                break;
            case 'out':
                const val2 = getValue(parts[1], registers);
                if (val2 !== 0 && val2 !== 1) return false;
                if (outputCount > 0 && val2 === lastOutput) return false;
                lastOutput = val2;
                outputCount++;
                if (outputCount > 50) return true;
                break;
        }
        i++;
    }
    return false;
}

function getValue(s: string, registers: { [key: string]: number }): number {
    const val = parseInt(s);
    return isNaN(val) ? registers[s] : val;
}

main();