const fs = require('fs');

const Size = 10007;

function main() {
  const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
  let deck = Array.from({ length: Size }, (_, i) => i);

  input.forEach((line) => {
    if (line === 'deal into new stack') {
      deck = dealIntoNewStack(deck);
    } else if (line.startsWith('cut')) {
      const n = parseInt(line.split(' ')[1]);
      deck = cutN(deck, n);
    } else if (line.startsWith('deal with increment')) {
      const n = parseInt(line.split(' ')[line.split(' ').length - 1]);
      deck = dealWithIncrement(deck, n);
    }
  });

  console.log(find2019(deck));
}

function dealIntoNewStack(deck) {
  for (let i = 0; i < Size / 2; i++) {
    [deck[i], deck[Size - i - 1]] = [deck[Size - i - 1], deck[i]];
  }
  return deck;
}

function cutN(deck, n) {
  if (n >= 0) {
    return deck.slice(n).concat(deck.slice(0, n));
  } else {
    return deck.slice(deck.length + n).concat(deck.slice(0, deck.length + n));
  }
}

function dealWithIncrement(deck, n) {
  const newDeck = Array.from({ length: Size });

  for (let i = 0; i < Size; i++) {
    newDeck[(i * n) % Size] = deck[i];
  }

  return newDeck;
}

function find2019(deck) {
  for (let i = 0; i < Size; i++) {
    if (deck[i] === 2019) {
      return i;
    }
  }
  return -1;
}

main();