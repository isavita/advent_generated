const fs = require('fs');

function parseInput(input) {
    const instructions = input[0];

    const nodes = {};
    for (let i = 2; i < input.length; i++) {
        const line = input[i];
        const [head, children] = parseLine(line);
        nodes[head] = children;
    }

    return {
        instructions,
        nodes
    };
}

function parseLine(line) {
    const parts = line.split(" = ");

    const head = parts[0];
    const childrenTrim = parts[1].replace("(", "").replace(")", "");
    const childrenParts = childrenTrim.split(", ");
    const children = [childrenParts[0], childrenParts[1]];

    return [head, children];
}

function gcd(a, b) {
    while (b !== 0) {
        [a, b] = [b, a % b];
    }
    return a;
}

function lcm(a, b) {
    return (a * b) / gcd(a, b);
}

function lcmArray(nums) {
    if (nums.length === 0) {
        return 0;
    }

    let res = nums[0];
    for (let i = 1; i < nums.length; i++) {
        res = lcm(res, nums[i]);
    }

    return res;
}

function solve(input) {
    const network = parseInput(input);

    const starts = [];
    for (const node in network.nodes) {
        const lastIndex = node.length - 1;
        if (node[lastIndex] === 'A') {
            starts.push(node);
        }
    }

    const steps = new Array(starts.length).fill(0);
    const instructionsLength = network.instructions.length;
    for (let i = 0; i < starts.length; i++) {
        let element = starts[i];
        let lastIndex = element.length - 1;
        while (element[lastIndex] !== 'Z') {
            const instruction = network.instructions[steps[i] % instructionsLength];
            if (instruction === 'L') {
                element = network.nodes[element][0];
            } else {
                element = network.nodes[element][1];
            }
            steps[i]++;
        }
    }

    const res = lcmArray(steps);
    return res;
}

function readFile(fileName) {
    const file = fs.readFileSync(fileName, 'utf8');
    return file.trim().split("\n");
}

const input = readFile("input.txt");
console.log(solve(input));