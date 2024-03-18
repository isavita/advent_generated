const fs = require('fs');

class Rule {
  constructor(category, operator, num, workflowName) {
    this.category = category;
    this.operator = operator;
    this.num = num;
    this.workflowName = workflowName;
  }
}

class Workflows {
  constructor() {
    this.workflows = new Map();
  }

  addWorkflow(workflowName, rules) {
    this.workflows.set(workflowName, rules);
  }

  getWorkflow(workflowName) {
    return this.workflows.get(workflowName);
  }
}

class Part {
  constructor(x, m, a, s) {
    this.x = x;
    this.m = m;
    this.a = a;
    this.s = s;
  }
}

function parseInput(input) {
  const workflows = new Workflows();
  const parts = [];

  let i = 0;
  for (; input[i] !== ''; i++) {
    const [workflowName, rules] = parseWorkflow(input[i]);
    workflows.addWorkflow(workflowName, rules);
  }

  for (i = i + 1; i < input.length; i++) {
    const part = parsePart(input[i]);
    parts.push(part);
  }

  return [workflows, parts];
}

function parseWorkflow(line) {
  const idx = line.indexOf('{');
  const workflowName = line.slice(0, idx);
  const rules = [];

  const rulesStr = line.slice(idx + 1, line.length - 1).split(',');
  for (const ruleStr of rulesStr) {
    const idx = ruleStr.indexOf(':');
    if (idx === -1) {
      rules.push(new Rule(null, null, null, ruleStr.trim()));
    } else {
      const category = ruleStr[0];
      const operator = ruleStr[1];
      const num = parseInt(ruleStr.slice(2, idx));
      const workflowName = ruleStr.slice(idx + 1).trim();
      rules.push(new Rule(category, operator, num, workflowName));
    }
  }

  return [workflowName, rules];
}

function parsePart(line) {
  const [, x, m, a, s] = line.match(/\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)\}/);
  return new Part(parseInt(x), parseInt(m), parseInt(a), parseInt(s));
}

function applyWorkflow(part, workflows, workflowName) {
  if (workflowName === 'A') {
    return true;
  }
  if (workflowName === 'R') {
    return false;
  }

  const rules = workflows.getWorkflow(workflowName);
  for (const rule of rules) {
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

function solve(input) {
  const startWorkflow = 'in';

  const [workflows, parts] = parseInput(input);

  let res = 0;
  for (const part of parts) {
    const isValid = applyWorkflow(part, workflows, startWorkflow);
    if (isValid) {
      res += part.x + part.m + part.a + part.s;
    }
  }

  return res;
}

function readFile(fileName) {
  return fs.readFileSync(fileName, 'utf-8').trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));