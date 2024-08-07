import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf8').trim();
const ans = solve(input);
console.log(ans);

function solve(input: string): number {
    const lines = parseInput(input);
    let total = 0;
    for (const line of lines) {
        total += doMaths(line, calcFlatSlicePart);
    }
    return total;
}

function parseInput(input: string): string[][] {
    const lines = input.split('\n');
    return lines.map(line => line.replace(/ /g, '').split(''));
}

function doMaths(input: string[], flatteningFunc: (input: string[]) => string): number {
    const stackOpenIndices: number[] = [];
    const stackFlattened: string[] = [];
    for (let i = 0; i < input.length; i++) {
        stackFlattened.push(input[i]);
        switch (input[i]) {
            case '(':
                stackOpenIndices.push(stackFlattened.length - 1);
                break;
            case ')':
                const openIndex = stackOpenIndices.pop() as number;
                const sliToFlatten = stackFlattened.slice(openIndex + 1, -1);
                stackFlattened[openIndex] = flatteningFunc(sliToFlatten);
                stackFlattened.splice(openIndex + 1, stackFlattened.length - openIndex - 1);
                break;
        }
    }
    return parseInt(flatteningFunc(stackFlattened), 10);
}

function calcFlatSlicePart1(input: string[]): string {
    for (const v of input) {
        if (v === '(' || v === ')') {
            throw new Error(`Unexpected paren in flat input: ${input}`);
        }
    }
    let result = parseInt(input[0], 10);
    for (let i = 0; i < input.length; i += 2) {
        if (input[i + 1] === '+') {
            result += parseInt(input[i + 2], 10);
        } else if (input[i + 1] === '*') {
            result *= parseInt(input[i + 2], 10);
        }
    }
    return result.toString();
}

function calcFlatSlicePart(input: string[]): string {
    for (const v of input) {
        if (v === '(' || v === ')') {
            throw new Error(`Unexpected paren in flat input: ${input}`);
        }
    }
    for (let i = 1; i < input.length - 1; i++) {
        if (input[i] === '+') {
            const toLeft = input[i - 1];
            const toRight = input[i + 1];
            if (!isNaN(parseInt(toLeft, 10)) && !isNaN(parseInt(toRight, 10))) {
                input[i - 1] = (parseInt(toLeft, 10) + parseInt(toRight, 10)).toString();
                input.splice(i, 2);
                i--;
            }
        }
    }
    for (let i = 1; i < input.length - 1; i++) {
        if (input[i] === '*') {
            const toLeft = input[i - 1];
            const toRight = input[i + 1];
            if (!isNaN(parseInt(toLeft, 10)) && !isNaN(parseInt(toRight, 10))) {
                input[i - 1] = (parseInt(toLeft, 10) * parseInt(toRight, 10)).toString();
                input.splice(i, 2);
                i--;
            }
        }
    }
    return input[0];
}