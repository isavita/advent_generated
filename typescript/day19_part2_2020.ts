import * as fs from 'fs';

interface Rule {
  resolved: string[];
  options: number[][];
}

function main() {
  const input = fs.readFileSync('input.txt', 'utf8').trim();
  const result = solve(input);
  console.log(result);
}

function solve(input: string): number {
  const [rules, messages] = parseInput(input);

  fillInGraph(rules, 42);
  fillInGraph(rules, 31);

  const part42 = `(${rules[42].resolved.join('|')})`;
  const part31 = `(${rules[31].resolved.join('|')})`;
  const rule8String = `(${part42})+`;

  const makeRegexp = (num: number) => new RegExp(`^${rule8String}${part42}{${num}}${part31}{${num}}$`);

  let matchRuleZero = 0;
  for (const m of messages) {
    for (let i = 1; i < 10; i++) {
      const pattern = makeRegexp(i);
      if (pattern.test(m)) {
        matchRuleZero++;
        break;
      }
    }
  }

  return matchRuleZero;
}

function fillInGraph(graph: { [key: number]: Rule }, entry: number): string[] {
  if (graph[entry].resolved.length !== 0) {
    return [...graph[entry].resolved];
  }

  for (const option of graph[entry].options) {
    let resolved = [''];
    for (const entryPoint of option) {
      const nestedResolveVals = fillInGraph(graph, entryPoint);
      const newResolved: string[] = [];
      for (const nextPiece of nestedResolveVals) {
        for (const prev of resolved) {
          newResolved.push(prev + nextPiece);
        }
      }
      resolved = newResolved;
    }
    graph[entry].resolved = [...graph[entry].resolved, ...resolved];
  }

  return graph[entry].resolved;
}

function parseInput(input: string): [{ [key: number]: Rule }, string[]] {
  const parts = input.split('\n\n');

  const rules: { [key: number]: Rule } = {};
  for (const r of parts[0].split('\n')) {
    if (/[a-z]/.test(r)) {
      const [num, char] = r.match(/(\d+): "(.)"/)!.slice(1);
      rules[parseInt(num)] = { resolved: [char], options: [] };
    } else {
      const [key, value] = r.split(': ');
      const newRule: Rule = { resolved: [], options: [] };
      for (const ruleNums of value.split(' | ')) {
        const nums = ruleNums.split(' ').map(Number);
        newRule.options.push(nums);
      }
      rules[parseInt(key)] = newRule;
    }
  }

  const messages = parts[1].split('\n');

  return [rules, messages];
}

main();