const fs = require('fs');

interface Network {
  instructions: string;
  nodes: { [key: string]: [string, string] };
}

function parseInput(input: string[]): Network {
  const instructions = input[0];
  const nodes: { [key: string]: [string, string] } = {};
  for (const line of input.slice(2)) {
    const [head, children] = parseLine(line);
    nodes[head] = children;
  }
  return { instructions, nodes };
}

function parseLine(line: string): [string, [string, string]] {
  const [head, childrenTrim] = line.split(' = ');
  const children = childrenTrim.trim().replace('(', '').replace(')', '').split(', ');
  return [head, children as [string, string]];
}

function gcd(a: number, b: number): number {
  while (b !== 0) {
    [a, b] = [b, a % b];
  }
  return a;
}

function lcm(a: number, b: number): number {
  return (a * b) / gcd(a, b);
}

function lcmSlice(nums: number[]): number {
  if (nums.length === 0) return 0;
  let res = nums[0];
  for (let i = 1; i < nums.length; i++) {
    res = lcm(res, nums[i]);
  }
  return res;
}

function solve(input: string[]): number {
  const network = parseInput(input);
  const starts = Object.keys(network.nodes).filter((node) => node.endsWith('A'));
  const steps = new Array(starts.length).fill(0);
  const instructionsLength = network.instructions.length;
  for (let i = 0; i < starts.length; i++) {
    let element = starts[i];
    while (!element.endsWith('Z')) {
      const instruction = network.instructions[steps[i] % instructionsLength];
      element = instruction === 'L' ? network.nodes[element][0] : network.nodes[element][1];
      steps[i]++;
    }
  }
  return lcmSlice(steps);
}

function readFile(fileName: string): string[] {
  return fs.readFileSync(fileName, 'utf8').trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));