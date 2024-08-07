import * as fs from 'fs';

function parseInput(filePath: string): [string, number, Map<string, Map<number, [number, number, string]>>] {
    const lines = fs.readFileSync(filePath, 'utf-8').split('\n');
    const initialState = lines[0][lines[0].length - 2];
    const steps = parseInt(lines[1].match(/\d+/)![0]);

    const states = new Map<string, Map<number, [number, number, string]>>();
    for (let i = 3; i < lines.length; i += 10) {
        const state = lines[i][lines[i].length - 2];
        const value0 = parseInt(lines[i + 2][lines[i + 2].length - 2]);
        const move0 = lines[i + 3].endsWith('left.') ? -1 : 1;
        const nextState0 = lines[i + 4][lines[i + 4].length - 2];
        const value1 = parseInt(lines[i + 6][lines[i + 6].length - 2]);
        const move1 = lines[i + 7].endsWith('left.') ? -1 : 1;
        const nextState1 = lines[i + 8][lines[i + 8].length - 2];
        states.set(state, new Map([[0, [value0, move0, nextState0]], [1, [value1, move1, nextState1]]]));
    }
    return [initialState, steps, states];
}

function runTuringMachine(filePath: string): number {
    const [initialState, steps, states] = parseInput(filePath);
    let state = initialState; // Change from const to let
    const tape = new Map<number, number>();
    let cursor = 0;
    let checksum = 0;

    for (let i = 0; i < steps; i++) {
        const value = tape.get(cursor) ?? 0;
        const [newValue, move, nextState] = states.get(state)!.get(value)!;

        tape.set(cursor, newValue);
        cursor += move;
        state = nextState; // This is now allowed because state is a let variable
    }

    for (const v of tape.values()) {
        checksum += v;
    }
    return checksum;
}

console.log(runTuringMachine('input.txt'));