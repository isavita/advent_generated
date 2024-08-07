import * as fs from 'fs';

// Function to perform a spin move
function spin(programs: string[], X: number): string[] {
    return [...programs.slice(-X), ...programs.slice(0, -X)];
}

// Function to perform an exchange move
function exchange(programs: string[], A: number, B: number): string[] {
    const temp = programs[A];
    programs[A] = programs[B];
    programs[B] = temp;
    return programs;
}

// Function to perform a partner move
function partner(programs: string[], A: string, B: string): string[] {
    const indexA = programs.indexOf(A);
    const indexB = programs.indexOf(B);
    return exchange(programs, indexA, indexB);
}

// Main function to process the dance moves
function dance(moves: string[]): string[] {
    let programs = 'abcdefghijklmnop'.split('');

    for (const move of moves) {
        if (move.startsWith('s')) {
            const X = parseInt(move.slice(1), 10);
            programs = spin(programs, X);
        } else if (move.startsWith('x')) {
            const [A, B] = move.slice(1).split('/').map(Number);
            programs = exchange(programs, A, B);
        } else if (move.startsWith('p')) {
            const [A, B] = move.slice(1).split('/');
            programs = partner(programs, A, B);
        }
    }

    return programs;
}

// Read input from file
const input = fs.readFileSync('input.txt', 'utf-8').trim().split(',');

// Perform the dance and print the result
const result = dance(input);
console.log(result.join(''));