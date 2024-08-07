import * as fs from 'fs';

type Instruction = {
    output: string;
    operation: (inputs: number[]) => number;
    inputs: string[];
};

const parseInput = (data: string): Instruction[] => {
    return data.trim().split('\n').map(line => {
        const [left, right] = line.split(' -> ');
        const output = right.trim();
        let operation: (inputs: number[]) => number;
        let inputs: string[];

        if (left.startsWith('NOT')) {
            inputs = [left.split(' ')[1].trim()];
            operation = ([input]) => ~input & 0xFFFF;
        } else if (left.includes('AND')) {
            inputs = left.split(' AND ').map(x => x.trim());
            operation = ([a, b]) => a & b;
        } else if (left.includes('OR')) {
            inputs = left.split(' OR ').map(x => x.trim());
            operation = ([a, b]) => a | b;
        } else if (left.includes('LSHIFT')) {
            const [input, shift] = left.split(' LSHIFT ');
            inputs = [input.trim(), shift.trim()];
            operation = ([value, shift]) => (value << shift) & 0xFFFF;
        } else if (left.includes('RSHIFT')) {
            const [input, shift] = left.split(' RSHIFT ');
            inputs = [input.trim(), shift.trim()];
            operation = ([value, shift]) => value >> shift;
        } else {
            inputs = [left.trim()];
            operation = ([value]) => value;
        }

        return { output, operation, inputs };
    });
};

const evaluateWire = (wire: string, instructions: Map<string, Instruction>, cache: Map<string, number>): number => {
    if (cache.has(wire)) return cache.get(wire)!;

    const instruction = instructions.get(wire);
    if (!instruction) throw new Error(`No instruction for wire: ${wire}`);

    const inputs = instruction.inputs.map(input => {
        const value = isNaN(Number(input)) ? evaluateWire(input, instructions, cache) : Number(input);
        return value & 0xFFFF;
    });

    const result = instruction.operation(inputs);
    cache.set(wire, result);
    return result;
};

const main = () => {
    const data = fs.readFileSync('input.txt', 'utf-8');
    const instructions = parseInput(data);
    const instructionMap = new Map(instructions.map(instr => [instr.output, instr]));
    const cache = new Map<string, number>();

    const result = evaluateWire('a', instructionMap, cache);
    console.log(result);
};

main();