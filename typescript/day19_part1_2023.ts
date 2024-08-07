import * as fs from 'fs';

type Rule = {
  category: string;
  operator: string;
  num: number;
  workflowName: string;
};

type Workflows = { [key: string]: Rule[] };

type Part = { [key: string]: number };

type Interval = { start: number; end: number };

type PartInterval = { [key: string]: Interval };

function parseInput(input: string[]): [Workflows, Part[]] {
  const workflows: Workflows = {};
  const parts: Part[] = [];

  let i = 0;
  for (; input[i] !== ''; i++) {
    const [workflowName, rules] = parseWorkflow(input[i]);
    workflows[workflowName] = rules;
  }

  for (i = i + 1; i < input.length; i++) {
    const part = parsePart(input[i]);
    parts.push(part);
  }

  return [workflows, parts];
}

function parseWorkflow(line: string): [string, Rule[]] {
  const idx = line.indexOf('{');
  const workflowName = line.slice(0, idx);
  const rulesStr = line.slice(idx + 1, line.length - 1).split(',');
  const rules: Rule[] = [];

  for (const ruleStr of rulesStr) {
    const rule: Rule = { category: '', operator: '', num: 0, workflowName: '' };
    const idx = ruleStr.indexOf(':');
    if (idx === -1) {
      rule.workflowName = ruleStr;
    } else {
      rule.category = ruleStr[0];
      rule.operator = ruleStr[1];
      rule.num = parseInt(ruleStr.slice(2, idx), 10);
      rule.workflowName = ruleStr.slice(idx + 1);
    }
    rules.push(rule);
  }

  return [workflowName, rules];
}

function parsePart(line: string): Part {
  const matches = line.match(/{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}/);
  if (!matches) throw new Error('Invalid part format');

  return {
    'x': parseInt(matches[1], 10),
    'm': parseInt(matches[2], 10),
    'a': parseInt(matches[3], 10),
    's': parseInt(matches[4], 10),
  };
}

function applyWorkflow(part: Part, workflows: Workflows, workflowName: string): boolean {
  if (workflowName === 'A') return true;
  if (workflowName === 'R') return false;

  for (const rule of workflows[workflowName]) {
    const rating = part[rule.category];
    let isValid = true;
    switch (rule.operator) {
      case '>':
        isValid = rating > rule.num;
        break;
      case '<':
        isValid = rating < rule.num;
        break;
      default:
        isValid = true;
    }

    if (isValid) {
      return applyWorkflow(part, workflows, rule.workflowName);
    }
  }

  return false;
}

function solve(input: string[]): number {
  const startWorkflow = 'in';
  const [workflows, parts] = parseInput(input);

  let res = 0;
  for (const part of parts) {
    const isValid = applyWorkflow(part, workflows, startWorkflow);
    if (isValid) {
      res += Object.values(part).reduce((sum, rating) => sum + rating, 0);
    }
  }

  return res;
}

function readFile(fileName: string): string[] {
  const data = fs.readFileSync(fileName, 'utf-8');
  return data.trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));