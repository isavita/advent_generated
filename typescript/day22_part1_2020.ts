import * as fs from 'fs';

function readDecks(filename: string): [number[], number[]] {
    const data = fs.readFileSync(filename, 'utf-8').trim().split('\n\n');
    const player1 = data[0].split('\n').slice(1).map(Number);
    const player2 = data[1].split('\n').slice(1).map(Number);
    return [player1, player2];
}

function calculateScore(deck: number[]): number {
    return deck.reduce((score, card, index) => score + card * (deck.length - index), 0);
}

function playGame(deck1: number[], deck2: number[]): number {
    while (deck1.length && deck2.length) {
        const card1 = deck1.shift()!;
        const card2 = deck2.shift()!;
        if (card1 > card2) {
            deck1.push(card1, card2);
        } else {
            deck2.push(card2, card1);
        }
    }
    return deck1.length > 0 ? calculateScore(deck1) : calculateScore(deck2);
}

const [deck1, deck2] = readDecks('input.txt');
const score = playGame(deck1, deck2);
console.log(score);