import * as fs from 'fs';

interface Player {
    score: number;
}

class Marble {
    value: number;
    prev: Marble | null;
    next: Marble | null;

    constructor(value: number) {
        this.value = value;
        this.prev = null;
        this.next = null;
    }
}

function playMarbleGame(numPlayers: number, lastMarble: number): number {
    const players: Player[] = Array.from({ length: numPlayers }, () => ({ score: 0 }));
    const firstMarble = new Marble(0);
    firstMarble.prev = firstMarble;
    firstMarble.next = firstMarble;

    let currentMarble: Marble = firstMarble;
    let currentPlayer = 0;

    for (let marble = 1; marble <= lastMarble; marble++) {
        if (marble % 23 === 0) {
            players[currentPlayer].score += marble;
            for (let i = 0; i < 7; i++) {
                currentMarble = currentMarble.prev!;
            }
            players[currentPlayer].score += currentMarble.value;
            currentMarble.prev!.next = currentMarble.next;
            currentMarble.next!.prev = currentMarble.prev;
            currentMarble = currentMarble.next!;
        } else {
            currentMarble = currentMarble.next!;
            const newMarble = new Marble(marble);
            newMarble.prev = currentMarble;
            newMarble.next = currentMarble.next;
            currentMarble.next!.prev = newMarble;
            currentMarble.next = newMarble;
            currentMarble = newMarble;
        }
        currentPlayer = (currentPlayer + 1) % numPlayers;
    }

    return Math.max(...players.map(p => p.score));
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const [numPlayers, lastMarble] = input.match(/\d+/g)!.map(Number);
    
    console.log(playMarbleGame(numPlayers, lastMarble * 100));
}

main();