const fs = require('fs');

class Deck {
  constructor(cards) {
    this.cards = cards;
  }

  copy(n) {
    return new Deck(this.cards.slice(0, n));
  }

  score() {
    return this.cards.reduce((score, card, index) => score + card * (this.cards.length - index), 0);
  }
}

function playRecursiveCombat(player1, player2) {
  const previousRounds = new Set();
  while (player1.cards.length > 0 && player2.cards.length > 0) {
    const roundKey = `${player1.cards.join('|')},${player2.cards.join('|')}`;
    if (previousRounds.has(roundKey)) {
      return [player1, new Deck([])];
    }
    previousRounds.add(roundKey);

    const card1 = player1.cards.shift();
    const card2 = player2.cards.shift();

    if (player1.cards.length >= card1 && player2.cards.length >= card2) {
      const [subPlayer1, _] = playRecursiveCombat(player1.copy(card1), player2.copy(card2));
      if (subPlayer1.cards.length > 0) {
        player1.cards.push(card1, card2);
      } else {
        player2.cards.push(card2, card1);
      }
    } else {
      if (card1 > card2) {
        player1.cards.push(card1, card2);
      } else {
        player2.cards.push(card2, card1);
      }
    }
  }
  return [player1, player2];
}

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
let player1Deck = new Deck([]);
let player2Deck = new Deck([]);
let currentDeck = player1Deck;

for (const line of input) {
  if (line === '') {
    currentDeck = player2Deck;
    continue;
  }
  if (line.includes('Player')) {
    continue;
  }
  const card = parseInt(line, 10);
  currentDeck.cards.push(card);
}

[player1Deck, player2Deck] = playRecursiveCombat(player1Deck, player2Deck);

const winningDeck = player1Deck.cards.length > 0 ? player1Deck : player2Deck;

console.log(winningDeck.score());