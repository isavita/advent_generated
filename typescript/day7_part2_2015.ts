import * as fs from 'fs';

function someAssemblyRequired(input: string): number {
  const wireToRule: { [wire: string]: string } = {};

  for (const inst of input.split('\n')) {
    const [rule, wire] = inst.split(' -> ');
    wireToRule[wire] = rule;
  }

  const aSignal = memoDFS(wireToRule, 'a', {});
  wireToRule['b'] = aSignal.toString();
  return memoDFS(wireToRule, 'a', {});
}

function memoDFS(graph: { [wire: string]: string }, entry: string, memo: { [wire: string]: number }): number {
  if (entry in memo) return memo[entry];

  if (!isNaN(parseInt(entry))) return parseInt(entry);

  const sourceRule = graph[entry];
  const parts = sourceRule.split(' ');

  let result: number;
  switch (true) {
    case parts.length === 1:
      result = memoDFS(graph, parts[0], memo);
      break;
    case parts[0] === 'NOT':
      result = ~memoDFS(graph, parts[1], memo) & 0xFFFF;
      break;
    case parts[1] === 'AND':
      result = memoDFS(graph, parts[0], memo) & memoDFS(graph, parts[2], memo);
      break;
    case parts[1] === 'OR':
      result = memoDFS(graph, parts[0], memo) | memoDFS(graph, parts[2], memo);
      break;
    case parts[1] === 'LSHIFT':
      result = memoDFS(graph, parts[0], memo) << memoDFS(graph, parts[2], memo);
      break;
    case parts[1] === 'RSHIFT':
      result = memoDFS(graph, parts[0], memo) >> memoDFS(graph, parts[2], memo);
      break;
    default:
      throw new Error('Unsupported operation');
  }

  memo[entry] = result;
  return result;
}

const input = fs.readFileSync('input.txt', 'utf8').trim();
console.log(someAssemblyRequired(input));