const fs = require('fs');

class Marble {
    constructor(value) {
        this.value = value;
        this.prev = null;
        this.next = null;
    }
}

function readInput(filename) {
    const input = fs.readFileSync(filename, 'utf8').trim().split('\n');
    const parts = input[0].split(' ');
    const players = parseInt(parts[0]);
    const lastMarble = parseInt(parts[6]);
    return [players, lastMarble];
}

function playMarbleGame(players, lastMarble) {
    const scores = new Array(players).fill(0);
    let current = new Marble(0);
    current.next = current;
    current.prev = current;

    for (let marble = 1; marble <= lastMarble; marble++) {
        if (marble % 23 === 0) {
            const player = marble % players;
            for (let i = 0; i < 7; i++) {
                current = current.prev;
            }
            scores[player] += marble + current.value;
            current.prev.next = current.next;
            current.next.prev = current.prev;
            current = current.next;
        } else {
            current = current.next;
            const newMarble = new Marble(marble);
            newMarble.prev = current;
            newMarble.next = current.next;
            current.next.prev = newMarble;
            current.next = newMarble;
            current = newMarble;
        }
    }

    let maxScore = 0;
    for (const score of scores) {
        if (score > maxScore) {
            maxScore = score;
        }
    }
    return maxScore;
}

const [players, lastMarble] = readInput('input.txt');
console.log(playMarbleGame(players, lastMarble));