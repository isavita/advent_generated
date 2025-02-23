
import * as fs from 'fs';

const instructions = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(line => line.split(' '));

function getValue(arg: string, registers: Record<string, number>): number {
    return isNaN(parseInt(arg)) ? (registers[arg] || 0) : parseInt(arg);
}

function solve(): number {
  const registers0: Record<string, number> = { p: 0 };
  const registers1: Record<string, number> = { p: 1 };
  const queue0: number[] = [];
  const queue1: number[] = [];
  let sendCount1 = 0;
  let i0 = 0, i1 = 0;
  let deadlock0 = false, deadlock1 = false;

    while (!(deadlock0 && deadlock1)) {
      deadlock0 = true;
      deadlock1 = true;

        while (i0 < instructions.length) {
            const [cmd, arg1, arg2] = instructions[i0];
            const val2 = arg2 ? getValue(arg2, registers0):0

            if (cmd === "snd") {
                queue1.push(getValue(arg1, registers0));
            } else if (cmd === "set") {
                registers0[arg1] = val2;
            } else if (cmd === "add") {
                registers0[arg1] = (registers0[arg1] || 0) + val2;
            } else if (cmd === "mul") {
                registers0[arg1] = (registers0[arg1] || 0) * val2;
            } else if (cmd === "mod") {
                registers0[arg1] = (registers0[arg1] || 0) % val2;
            } else if (cmd === "rcv") {
                if (queue0.length === 0) {
                    break;
                }
                registers0[arg1] = queue0.shift()!;
            } else if (cmd === "jgz") {
                if (getValue(arg1, registers0) > 0) {
                    i0 += val2;
                    continue;
                }
            }
            i0++;
            deadlock0 = false;
        }

        while (i1 < instructions.length) {
          const [cmd, arg1, arg2] = instructions[i1];
          const val2 = arg2 ? getValue(arg2, registers1):0;

            if (cmd === "snd") {
                queue0.push(getValue(arg1, registers1));
                sendCount1++;
            } else if (cmd === "set") {
                registers1[arg1] = val2;
            } else if (cmd === "add") {
                registers1[arg1] = (registers1[arg1] || 0) + val2;
            } else if (cmd === "mul") {
                registers1[arg1] = (registers1[arg1] || 0) * val2;
            } else if (cmd === "mod") {
                registers1[arg1] = (registers1[arg1] || 0) % val2;
            } else if (cmd === "rcv") {
                if (queue1.length === 0) {
                    break;
                }
                registers1[arg1] = queue1.shift()!;
            } else if (cmd === "jgz") {
                if (getValue(arg1, registers1) > 0) {
                    i1 += val2;
                    continue;
                }
            }
            i1++;
          deadlock1 = false
        }
    }

    return sendCount1;
}

console.log(solve());
