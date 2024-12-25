
import * as fs from 'fs';

function permutations(arr: number[]): number[][] {
  const res: number[][] = [];
  function helper(arr: number[], n: number) {
    if (n === 1) {
      res.push([...arr]);
    } else {
      for (let i = 0; i < n; i++) {
        helper(arr, n - 1);
        const temp = arr[n % 2 === 1 ? i : 0];
        arr[n % 2 === 1 ? i : 0] = arr[n - 1];
        arr[n - 1] = temp;
      }
    }
  }
  helper(arr, arr.length);
  return res;
}

function runVM(code: number[], input: number[], output: number[]) {
  let ip = 0;
  let inputIndex = 0;

  function getParam(param: number, immediate: boolean): number {
    return immediate ? code[param] : code[code[param]];
  }

  while (true) {
    const cmd = code[ip];
    const opCode = cmd % 100;

    switch (opCode) {
      case 1: {
        const param1 = getParam(ip + 1, Math.floor(cmd / 100) % 10 === 1);
        const param2 = getParam(ip + 2, Math.floor(cmd / 1000) % 10 === 1);
        code[code[ip + 3]] = param1 + param2;
        ip += 4;
        break;
      }
      case 2: {
        const param1 = getParam(ip + 1, Math.floor(cmd / 100) % 10 === 1);
        const param2 = getParam(ip + 2, Math.floor(cmd / 1000) % 10 === 1);
        code[code[ip + 3]] = param1 * param2;
        ip += 4;
        break;
      }
      case 3: {
        code[code[ip + 1]] = input[inputIndex++];
        ip += 2;
        break;
      }
      case 4: {
        const param1 = getParam(ip + 1, Math.floor(cmd / 100) % 10 === 1);
        output.push(param1);
        ip += 2;
        break;
      }
      case 5: {
        const param1 = getParam(ip + 1, Math.floor(cmd / 100) % 10 === 1);
        const param2 = getParam(ip + 2, Math.floor(cmd / 1000) % 10 === 1);
        ip = param1 !== 0 ? param2 : ip + 3;
        break;
      }
      case 6: {
        const param1 = getParam(ip + 1, Math.floor(cmd / 100) % 10 === 1);
        const param2 = getParam(ip + 2, Math.floor(cmd / 1000) % 10 === 1);
        ip = param1 === 0 ? param2 : ip + 3;
        break;
      }
      case 7: {
        const param1 = getParam(ip + 1, Math.floor(cmd / 100) % 10 === 1);
        const param2 = getParam(ip + 2, Math.floor(cmd / 1000) % 10 === 1);
        code[code[ip + 3]] = param1 < param2 ? 1 : 0;
        ip += 4;
        break;
      }
      case 8: {
        const param1 = getParam(ip + 1, Math.floor(cmd / 100) % 10 === 1);
        const param2 = getParam(ip + 2, Math.floor(cmd / 1000) % 10 === 1);
        code[code[ip + 3]] = param1 === param2 ? 1 : 0;
        ip += 4;
        break;
      }
      case 99:
        return;
      default:
        throw new Error(`Invalid opcode: ${opCode}`);
    }
  }
}

function runLoop(phase: number[], code: number[]): number {
  const outputs: number[][] = [[], [], [], [], []];
  const inputs: number[][] = [[phase[0], 0], [phase[1]], [phase[2]], [phase[3]], [phase[4]]];

  const codes: number[][] = [];
  for (let i = 0; i < 5; i++) {
    codes.push([...code]);
  }

  const ips: number[] = [0, 0, 0, 0, 0];
  const inputIndexes: number[] = [0, 0, 0, 0, 0];

  function getParam(code: number[], ip: number, param: number, immediate: boolean): number {
    return immediate ? code[param] : code[code[param]];
  }

  let halted = [false, false, false, false, false];
  let amp = 0;

  while (halted.some(h => !h)) {
    let ip = ips[amp];
    let inputIndex = inputIndexes[amp];
    let currentCode = codes[amp];

    while (true) {
      const cmd = currentCode[ip];
      const opCode = cmd % 100;

      if (opCode === 99) {
        halted[amp] = true;
        break;
      }

      if (opCode === 3 && inputIndex >= inputs[amp].length) {
        ips[amp] = ip;
        inputIndexes[amp] = inputIndex;
        break;
      }

      switch (opCode) {
        case 1: {
          const param1 = getParam(currentCode, ip, ip + 1, Math.floor(cmd / 100) % 10 === 1);
          const param2 = getParam(currentCode, ip, ip + 2, Math.floor(cmd / 1000) % 10 === 1);
          currentCode[currentCode[ip + 3]] = param1 + param2;
          ip += 4;
          break;
        }
        case 2: {
          const param1 = getParam(currentCode, ip, ip + 1, Math.floor(cmd / 100) % 10 === 1);
          const param2 = getParam(currentCode, ip, ip + 2, Math.floor(cmd / 1000) % 10 === 1);
          currentCode[currentCode[ip + 3]] = param1 * param2;
          ip += 4;
          break;
        }
        case 3: {
          currentCode[currentCode[ip + 1]] = inputs[amp][inputIndex++];
          ip += 2;
          break;
        }
        case 4: {
          const param1 = getParam(currentCode, ip, ip + 1, Math.floor(cmd / 100) % 10 === 1);
          outputs[amp].push(param1);
          inputs[(amp + 1) % 5].push(param1);
          ip += 2;
          break;
        }
        case 5: {
          const param1 = getParam(currentCode, ip, ip + 1, Math.floor(cmd / 100) % 10 === 1);
          const param2 = getParam(currentCode, ip, ip + 2, Math.floor(cmd / 1000) % 10 === 1);
          ip = param1 !== 0 ? param2 : ip + 3;
          break;
        }
        case 6: {
          const param1 = getParam(currentCode, ip, ip + 1, Math.floor(cmd / 100) % 10 === 1);
          const param2 = getParam(currentCode, ip, ip + 2, Math.floor(cmd / 1000) % 10 === 1);
          ip = param1 === 0 ? param2 : ip + 3;
          break;
        }
        case 7: {
          const param1 = getParam(currentCode, ip, ip + 1, Math.floor(cmd / 100) % 10 === 1);
          const param2 = getParam(currentCode, ip, ip + 2, Math.floor(cmd / 1000) % 10 === 1);
          currentCode[currentCode[ip + 3]] = param1 < param2 ? 1 : 0;
          ip += 4;
          break;
        }
        case 8: {
          const param1 = getParam(currentCode, ip, ip + 1, Math.floor(cmd / 100) % 10 === 1);
          const param2 = getParam(currentCode, ip, ip + 2, Math.floor(cmd / 1000) % 10 === 1);
          currentCode[currentCode[ip + 3]] = param1 === param2 ? 1 : 0;
          ip += 4;
          break;
        }
      }
    }
    amp = (amp + 1) % 5;
  }

  return outputs[4][outputs[4].length - 1];
}

const fileContent = fs.readFileSync('input.txt', 'utf-8');
const code = fileContent.trim().split(',').map(Number);

let max = 0;
for (const phase of permutations([5, 6, 7, 8, 9])) {
  const res = runLoop(phase, code);
  max = Math.max(max, res);
}

console.log(max);
