const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split(',');

const programs = 'abcdefghijklmnop'.split('');
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
        const [a, b] = move.slice(1).split('/');
        partner(programs, a, b);
        break;
    }
  }

  if (programs.join('') === initial) {
    cycleLen = i + 1;
    break;
  }
}

programs.length = 0;
programs.push(...initial.split(''));

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
        const [a, b] = move.slice(1).split('/');
        partner(programs, a, b);
        break;
    }
  }
}

console.log(programs.join(''));

function spin(programs, x) {
  const n = programs.length;
  const temp = programs.slice();
  for (let i = 0; i < n; i++) {
    programs[(i + x) % n] = temp[i];
  }
}

function exchange(programs, A, B) {
  [programs[A], programs[B]] = [programs[B], programs[A]];
}

function partner(programs, A, B) {
  const indexA = programs.indexOf(A);
  const indexB = programs.indexOf(B);
  exchange(programs, indexA, indexB);
}