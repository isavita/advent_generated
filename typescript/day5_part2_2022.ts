import * as fs from 'fs';

const readAll = (path: string): string => fs.readFileSync(path, 'utf-8').trim();

const main = () => {
  const s = readAll("input.txt").split("\n\n");
  const input = s[0].split("\n");
  const stacks: string[][] = Array.from({ length: (input[0].length + 1) / 4 }, () => []);

  for (const line of input) {
    for (let i = 0; i < line.length; i++) {
      if (line[i] >= 'A' && line[i] <= 'Z') {
        stacks[(i - 1) / 4].push(line[i]);
      }
    }
  }

  const steps = s[1].split("\n");
  console.log(move(stacks, steps));
};

const move = (st: string[][], steps: string[]): string => {
  const stacks = st.map(stack => stack.reverse());

  for (const step of steps) {
    const [_, n, from, to] = step.match(/move (\d+) from (\d+) to (\d+)/)!.map(Number);
    stacks[to - 1].push(...stacks[from - 1].splice(-n));
  }

  return stacks.map(stack => stack[stack.length - 1]).join('');
};

main();