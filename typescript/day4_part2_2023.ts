import * as fs from 'fs';

interface Card {
  winnings: { [key: string]: number };
  givens: { [key: string]: number };
  totalCount: number;
}

function getPointsForCard(card: Card): number {
  let points = 0;
  for (const given in card.givens) {
    if (card.winnings[given]) {
      points += card.givens[given] * card.winnings[given];
    }
  }
  return points;
}

function lexLineIntoCard(line: string): Card {
  const cardDataStr = line.split(': ')[1];
  const cardData = cardDataStr.split(' | ');

  const winnings: { [key: string]: number } = {};
  const givens: { [key: string]: number } = {};

  for (const point of cardData[0].match(/[0-9]{1,2}/g)!) {
    winnings[point] = (winnings[point] || 0) + 1;
  }

  for (const point of cardData[1].match(/[0-9]{1,2}/g)!) {
    givens[point] = (givens[point] || 0) + 1;
  }

  return {
    winnings,
    givens,
    totalCount: 1,
  };
}

fs.readFile('input.txt', 'utf8', (err: NodeJS.ErrnoException | null, input: string) => {
  if (err) {
    console.error('Error reading file:', err);
    return;
  }

  const lines = input.trim().split('\n');
  const cards: Card[] = [];

  for (const line of lines) {
    if (line.trim() === '') continue;
    cards.push(lexLineIntoCard(line));
  }

  for (let i = 0; i < cards.length; i++) {
    const points = getPointsForCard(cards[i]);
    for (let j = 1; j <= points; j++) {
      if (i + j < cards.length) {
        cards[i + j].totalCount += cards[i].totalCount;
      }
    }
  }

  const totalCards = cards.reduce((sum, card) => sum + card.totalCount, 0);
  console.log(totalCards);
});