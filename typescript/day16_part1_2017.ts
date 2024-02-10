const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split(',');

let programs = Array.from({ length: 16 }, (_, i) => String.fromCharCode(97 + i));

for (const move of input) {
    switch (move[0]) {
        case 's':
            const x = parseInt(move.slice(1));
            programs = spin(programs, x);
            break;
        case 'x':
            const [A, B] = move.slice(1).split('/').map(Number);
            programs = exchange(programs, A, B);
            break;
        case 'p':
            const [A1, B1] = move.slice(1).split('/');
            programs = partner(programs, A1, B1);
            break;
    }
}

console.log(programs.join(''));

function spin(programs, x) {
    const n = programs.length;
    const temp = programs.slice();

    for (let i = 0; i < n; i++) {
        programs[(i + x) % n] = temp[i];
    }

    return programs;
}

function exchange(programs, A, B) {
    [programs[A], programs[B]] = [programs[B], programs[A]];
    return programs;
}

function partner(programs, A, B) {
    const indexA = programs.indexOf(A);
    const indexB = programs.indexOf(B);
    [programs[indexA], programs[indexB]] = [programs[indexB], programs[indexA]];
    return programs;
}