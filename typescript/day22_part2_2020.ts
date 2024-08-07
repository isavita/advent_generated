import * as fs from 'fs';

class Deck {
  private cards: number[];

  constructor(cards: number[]) {
    this.cards = cards;
  }

  public copy(n: number): Deck {
    return new Deck(this.cards.slice(0, n));
  }

  public score(): number {
    return this.cards.reduce((acc: number, card: number, i: number) => acc + card * (this.cards.length - i), 0);
  }

  public getCards(): number[] {
    return this.cards;
  }
}

function playRecursiveCombat(player1: Deck, player2: Deck): [Deck, Deck] {
  const previousRounds: Set<string> = new Set();
  while (player1.getCards().length > 0 && player2.getCards().length > 0) {
    const roundKey: string = `${player1.getCards().join(',')}|${player2.getCards().join(',')}`;
    if (previousRounds.has(roundKey)) {
      return [player1, new Deck([])];
    }
    previousRounds.add(roundKey);

    const card1: number = player1.getCards().shift() as number;
    const card2: number = player2.getCards().shift() as number;

    if (player1.getCards().length >= card1 && player2.getCards().length >= card2) {
      const subPlayer1: Deck = playRecursiveCombat(player1.copy(card1), player2.copy(card2))[0];
      if (subPlayer1.getCards().length > 0) {
        player1.getCards().push(card1, card2);
      } else {
        player2.getCards().push(card2, card1);
      }
    } else {
      if (card1 > card2) {
        player1.getCards().push(card1, card2);
      } else {
        player2.getCards().push(card2, card1);
      }
    }
  }
  return [player1, player2];
}

const input: string[] = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const player1Deck: number[] = [];
const player2Deck: number[] = [];
let currentDeck: number[] = player1Deck;

input.forEach((line: string) => {
  if (line === '') {
    currentDeck = player2Deck;
  } else if (!line.includes('Player')) {
    currentDeck.push(parseInt(line));
  }
});

const [player1, player2] = playRecursiveCombat(new Deck(player1Deck), new Deck(player2Deck));
const winningDeck: Deck = player1.getCards().length > 0 ? player1 : player2;
console.log(winningDeck.score());