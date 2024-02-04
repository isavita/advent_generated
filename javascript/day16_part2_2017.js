const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split(',');

let programs = Array.from({ length: 16 }, (_, i) => String.fromCharCode(97 + i));
const initial = programs.join('');
let cycleLen = 0;

for (let i = 0; i < 1000000000; i++) {
    for (const move of input) {
        switch (move[0]) {
            case 's':
                const x = parseInt(move.slice(1));
                spin(programs, x);
                break;
            case 'x':
                const [A, B] = move.slice(1).split('/').map(Number);
                exchange(programs, A, B);
                break;
            case 'p':
                const [AIndex, BIndex] = move.slice(1).split('/').map(p => programs.indexOf(p));
                partner(programs, AIndex, BIndex);
                break;
        }
    }

    if (programs.join('') === initial) {
        cycleLen = i + 1;
        break;
    }
}

programs = Array.from({ length: 16 }, (_, i) => String.fromCharCode(97 + i));

for (let i = 0; i < 1000000000 % cycleLen; i++) {
    for (const move of input) {
        switch (move[0]) {
            case 's':
                const x = parseInt(move.slice(1));
                spin(programs, x);
                break;
            case 'x':
                const [A, B] = move.slice(1).split('/').map(Number);
                exchange(programs, A, B);
                break;
            case 'p':
                const [AIndex, BIndex] = move.slice(1).split('/').map(p => programs.indexOf(p));
                partner(programs, AIndex, BIndex);
                break;
        }
    }
}

console.log(programs.join(''));

function spin(programs, x) {
    const n = programs.length;
    const temp = [...programs];

    for (let i = 0; i < n; i++) {
        programs[(i + x) % n] = temp[i];
    }
}

function exchange(programs, A, B) {
    [programs[A], programs[B]] = [programs[B], programs[A]];
}

function partner(programs, AIndex, BIndex) {
    [programs[AIndex], programs[BIndex]] = [programs[BIndex], programs[AIndex]];
}