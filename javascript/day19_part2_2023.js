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

  parseWorkflow(line) {
    const idx = line.indexOf('{');
    const workflowName = line.slice(0, idx);
    const rules = [];

    const rulesStr = line.slice(idx + 1, line.length - 1).split(',');
    for (const ruleStr of rulesStr) {
      const idx = ruleStr.indexOf(':');
      if (idx === -1) {
        rules.push(new Rule(undefined, undefined, undefined, ruleStr.trim()));
      } else {
        const category = ruleStr[0];
        const operator = ruleStr[1];
        const num = parseInt(ruleStr.slice(2, idx));
        const workflowName = ruleStr.slice(idx + 1).trim();
        rules.push(new Rule(category, operator, num, workflowName));
      }
    }

    this.workflows.set(workflowName, rules);
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

class Interval {
  constructor(start, end) {
    this.start = start;
    this.end = end;
  }
}

class PartInterval {
  constructor() {
    this.intervals = new Map();
  }

  set(category, interval) {
    this.intervals.set(category, interval);
  }

  get(category) {
    return this.intervals.get(category);
  }
}

function parseInput(input) {
  const workflows = new Workflows();
  const parts = [];

  let i = 0;
  for (; input[i] !== ''; i++) {
    workflows.parseWorkflow(input[i]);
  }

  for (i = i + 1; i < input.length; i++) {
    const [x, m, a, s] = input[i].match(/\d+/g).map(Number);
    parts.push(new Part(x, m, a, s));
  }

  return [workflows, parts];
}

function applyWorkflow(part, workflows, workflowName) {
  if (workflowName === 'A') {
    return true;
  }
  if (workflowName === 'R') {
    return false;
  }

  const rules = workflows.workflows.get(workflowName);
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

function applyWorkflowInterval(partInterval, workflows, workflowName) {
  if (workflowName === 'A') {
    let res = 1;
    for (const interval of partInterval.intervals.values()) {
      res *= interval.end - interval.start + 1;
    }
    return res;
  }
  if (workflowName === 'R') {
    return 0;
  }

  let res = 0;
  const rules = workflows.workflows.get(workflowName);
  for (const rule of rules) {
    const ratingInterval = partInterval.get(rule.category);
    let validRatingInterval, invalidRatingInterval;

    switch (rule.operator) {
      case '>':
        invalidRatingInterval = new Interval(ratingInterval.start, rule.num);
        validRatingInterval = new Interval(rule.num + 1, ratingInterval.end);
        break;
      case '<':
        validRatingInterval = new Interval(ratingInterval.start, rule.num - 1);
        invalidRatingInterval = new Interval(rule.num, ratingInterval.end);
        break;
      default:
        validRatingInterval = ratingInterval;
    }

    const newPartInterval = new PartInterval();
    for (const [category, interval] of partInterval.intervals) {
      if (category === rule.category) {
        newPartInterval.set(category, validRatingInterval);
      } else {
        newPartInterval.set(category, interval);
      }
    }
    res += applyWorkflowInterval(newPartInterval, workflows, rule.workflowName);

    partInterval.set(rule.category, invalidRatingInterval);
  }

  return res;
}

function solve(input) {
  const startWorkflow = 'in';
  const minRating = 1;
  const maxRating = 4000;

  const [workflows, parts] = parseInput(input);
  const partInterval = new PartInterval();
  partInterval.set('x', new Interval(minRating, maxRating));
  partInterval.set('m', new Interval(minRating, maxRating));
  partInterval.set('a', new Interval(minRating, maxRating));
  partInterval.set('s', new Interval(minRating, maxRating));

  const res = applyWorkflowInterval(partInterval, workflows, startWorkflow);
  return res;
}

function readFile(fileName) {
  return fs.readFileSync(fileName, 'utf-8').trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));