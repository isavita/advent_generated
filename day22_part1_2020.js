const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

let player1Deck = [];
let player2Deck = [];
let currentDeck = player1Deck;

for (let line of input) {
    if (line === "") {
        currentDeck = player2Deck;
        continue;
    }
    if (line.includes("Player")) {
        continue;
    }
    currentDeck.push(parseInt(line));
}

while (player1Deck.length > 0 && player2Deck.length > 0) {
    const card1 = player1Deck.shift();
    const card2 = player2Deck.shift();
    if (card1 > card2) {
        player1Deck.push(card1, card2);
    } else {
        player2Deck.push(card2, card1);
    }
}

let winningDeck = player1Deck.length > 0 ? player1Deck : player2Deck;

let score = 0;
for (let i = 0; i < winningDeck.length; i++) {
    score += winningDeck[i] * (winningDeck.length - i);
}

console.log(score);