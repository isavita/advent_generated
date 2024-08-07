import * as fs from 'fs';

const programs = 'abcdefghijklmnop'.split('');
const input = fs.readFileSync('input.txt', 'utf-8').trim().split(',');

function dance(moves: string[], positions: string[]): string[] {
    for (const move of moves) {
        if (move.startsWith('s')) {
            const x = parseInt(move.slice(1), 10);
            const spin = positions.splice(-x);
            positions = spin.concat(positions);
        } else if (move.startsWith('x')) {
            const [a, b] = move.slice(1).split('/').map(Number);
            [positions[a], positions[b]] = [positions[b], positions[a]];
        } else if (move.startsWith('p')) {
            const [a, b] = move.slice(1).split('/').map(c => positions.indexOf(c));
            [positions[a], positions[b]] = [positions[b], positions[a]];
        }
    }
    return positions;
}

const initialState = [...programs];
const seenStates = new Map<string, number>();
let currentState = initialState.join('');
seenStates.set(currentState, 0);

for (let i = 0; i < 1000000000; i++) {
    currentState = dance(input, [...currentState]).join('');
    if (seenStates.has(currentState)) {
        const cycleLength = i + 1 - seenStates.get(currentState)!;
        const remaining = (1000000000 - (i + 1)) % cycleLength;
        for (const [state, iteration] of seenStates.entries()) {
            if (iteration === remaining) {
                console.log(state);
                break; // Exit the loop after printing the state
            }
        }
        break; // Exit the main loop
    }
    seenStates.set(currentState, i + 1);
}

console.log(currentState);