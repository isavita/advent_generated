let fs = require('fs');

let input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
let happiness = {};

input.forEach(line => {
    let words = line.split(' ');
    let person1 = words[0];
    let person2 = words[10].slice(0, -1);
    let value = parseInt(words[3]);
    if (words[2] === 'lose') {
        value = -value;
    }

    if (!happiness[person1]) {
        happiness[person1] = {};
    }
    happiness[person1][person2] = value;
});

let people = Object.keys(happiness);
let permutations = getPermutations(people);
let maxHappiness = 0;

permutations.forEach(perm => {
    let happinessChange = calculateHappinessChange(perm, happiness);
    maxHappiness = Math.max(maxHappiness, happinessChange);
});

console.log(maxHappiness);

function getPermutations(arr) {
    let permutations = [];

    if (arr.length === 1) {
        return [arr];
    }

    for (let i = 0; i < arr.length; i++) {
        let firstElem = arr[i];
        let rest = [...arr.slice(0, i), ...arr.slice(i + 1)];
        let innerPermutations = getPermutations(rest);

        for (let j = 0; j < innerPermutations.length; j++) {
            permutations.push([firstElem, ...innerPermutations[j]]);
        }
    }

    return permutations;
}

function calculateHappinessChange(arrangement, happinessMap) {
    let totalChange = 0;

    for (let i = 0; i < arrangement.length; i++) {
        let person = arrangement[i];
        let nextPerson = arrangement[(i + 1) % arrangement.length];
        let prevPerson = arrangement[(i + arrangement.length - 1) % arrangement.length];

        totalChange += happinessMap[person][nextPerson];
        totalChange += happinessMap[person][prevPerson];
    }

    return totalChange;
}