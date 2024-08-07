import * as fs from 'fs';

type Rule = {
    category: string;
    operator: string;
    num: number;
    workflowName: string;
};

type Workflows = {
    [key: string]: Rule[];
};

type Part = {
    [key: string]: number;
};

type Interval = {
    start: number;
    end: number;
};

type PartInterval = {
    [key: string]: Interval;
};

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
    const workflowName = line.substring(0, idx);
    const rules: Rule[] = [];

    const rulesStr = line.substring(idx + 1, line.length - 1).split(',');
    for (const ruleStr of rulesStr) {
        const idx = ruleStr.indexOf(':');
        const rule: Rule = {
            category: '',
            operator: '',
            num: 0,
            workflowName: ''
        };
        if (idx === -1) {
            rule.workflowName = ruleStr;
        } else {
            rule.category = ruleStr[0];
            rule.operator = ruleStr[1];
            rule.num = parseInt(ruleStr.substring(2, idx), 10);
            rule.workflowName = ruleStr.substring(idx + 1);
        }
        rules.push(rule);
    }

    return [workflowName, rules];
}

function parsePart(line: string): Part {
    const match = line.match(/{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}/);
    if (!match) throw new Error('Invalid part format');

    return {
        'x': parseInt(match[1], 10),
        'm': parseInt(match[2], 10),
        'a': parseInt(match[3], 10),
        's': parseInt(match[4], 10),
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

function applyWorkflowInterval(partInterval: PartInterval, workflows: Workflows, workflowName: string): number {
    if (workflowName === 'A') {
        let res = 1;
        for (const interval of Object.values(partInterval)) {
            res *= interval.end - interval.start + 1;
        }
        return res;
    }
    if (workflowName === 'R') {
        return 0;
    }

    let res = 0;
    for (const rule of workflows[workflowName]) {
        const ratingInterval = partInterval[rule.category];
        let validRatingInterval: Interval;
        let invalidRatingInterval: Interval;

        switch (rule.operator) {
            case '>':
                invalidRatingInterval = { start: ratingInterval.start, end: rule.num };
                validRatingInterval = { start: rule.num + 1, end: ratingInterval.end };
                break;
            case '<':
                validRatingInterval = { start: ratingInterval.start, end: rule.num - 1 };
                invalidRatingInterval = { start: rule.num, end: ratingInterval.end };
                break;
            default:
                validRatingInterval = ratingInterval;
                invalidRatingInterval = ratingInterval; // Fix the unassigned variable issue
        }

        const newPart: PartInterval = {};
        for (const key of Object.keys(partInterval)) {
            if (key === rule.category) {
                newPart[key] = validRatingInterval;
            } else {
                newPart[key] = partInterval[key];
            }
        }
        res += applyWorkflowInterval(newPart, workflows, rule.workflowName);
        partInterval[rule.category] = invalidRatingInterval;
    }

    return res;
}

function solve(input: string[]): number {
    const startWorkflow = 'in';
    const minRating = 1;
    const maxRating = 4000;

    const [workflows] = parseInput(input);
    const partInterval: PartInterval = {
        'x': { start: minRating, end: maxRating },
        'm': { start: minRating, end: maxRating },
        'a': { start: minRating, end: maxRating },
        's': { start: minRating, end: maxRating },
    };

    return applyWorkflowInterval(partInterval, workflows, startWorkflow);
}

function readFile(fileName: string): string[] {
    return fs.readFileSync(fileName, 'utf-8').trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));