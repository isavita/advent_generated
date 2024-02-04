
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const target = {
    children: 3,
    cats: 7,
    samoyeds: 2,
    pomeranians: 3,
    akitas: 0,
    vizslas: 0,
    goldfish: 5,
    trees: 3,
    cars: 2,
    perfumes: 1
};

let sueNumber = 0;

for (let i = 0; i < input.length; i++) {
    const sue = input[i].split(' ');
    let match = true;

    for (let j = 2; j < sue.length; j += 2) {
        const prop = sue[j].replace(':', '');
        const value = parseInt(sue[j + 1]);

        if (target[prop] !== value) {
            match = false;
            break;
        }
    }

    if (match) {
        sueNumber = parseInt(sue[1]);
        break;
    }
}

console.log(sueNumber);
