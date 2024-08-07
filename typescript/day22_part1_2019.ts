import * as fs from 'fs';

const deckSize = 10007;

function shuffleDeck(instructions: string[]): number[] {
    let deck = Array.from({ length: deckSize }, (_, i) => i);

    for (const instruction of instructions) {
        if (instruction.startsWith('deal into new stack')) {
            deck.reverse();
        } else if (instruction.startsWith('cut')) {
            const n = parseInt(instruction.split(' ')[1], 10);
            if (n >= 0) {
                deck = [...deck.slice(n), ...deck.slice(0, n)];
            } else {
                deck = [...deck.slice(deckSize + n), ...deck.slice(0, deckSize + n)];
            }
        } else if (instruction.startsWith('deal with increment')) {
            const n = parseInt(instruction.split(' ')[3], 10);
            const newDeck = Array(deckSize).fill(0);
            let pos = 0;

            for (const card of deck) {
                newDeck[pos] = card;
                pos = (pos + n) % deckSize;
            }
            deck = newDeck;
        }
    }

    return deck;
}

function findCardPosition(deck: number[], card: number): number {
    return deck.indexOf(card);
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) throw err;
    const instructions = data.trim().split('\n');
    const shuffledDeck = shuffleDeck(instructions);
    const position = findCardPosition(shuffledDeck, 2019);
    console.log(position);
});