const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const deckSize = 10007;
let cards = Array.from({length: deckSize}, (_, i) => i);

input.forEach(instruction => {
    if (instruction === 'deal into new stack') {
        cards = cards.reverse();
    } else if (instruction.startsWith('cut')) {
        const n = parseInt(instruction.split(' ')[1]);
        cards = cards.slice(n).concat(cards.slice(0, n));
    } else if (instruction.startsWith('deal with increment')) {
        const n = parseInt(instruction.split(' ')[3]);
        const newCards = Array(deckSize);
        let index = 0;
        cards.forEach(card => {
            newCards[index] = card;
            index = (index + n) % deckSize;
        });
        cards = newCards;
    }
});

console.log(cards.indexOf(2019));