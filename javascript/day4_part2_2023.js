const fs = require('fs');

class Card {
    constructor(winnings, givens, totalCount) {
        this.winnings = winnings;
        this.givens = givens;
        this.totalCount = totalCount;
    }
}

function getPointsForCard(card) {
    let points = 0;
    for (let given in card.givens) {
        if (card.winnings.hasOwnProperty(given)) {
            points += card.givens[given] * card.winnings[given];
        }
    }
    return points;
}

function lexLineIntoCard(line) {
    const [_, cardDataStr, __] = line.split(": ");
    const cardData = cardDataStr.split(" | ");

    const re = /[0-9]{1,2}/g;

    const winnings = {};
    cardData[0].match(re).forEach(point => {
        winnings[point] = (winnings[point] || 0) + 1;
    });

    const givens = {};
    cardData[1].match(re).forEach(point => {
        givens[point] = (givens[point] || 0) + 1;
    });

    return new Card(winnings, givens, 1);
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error("Error reading file:", err);
        return;
    }

    const input = data.trim();
    const cards = [];

    input.split("\n").forEach(line => {
        if (line.length === 0) {
            return;
        }
        const card = lexLineIntoCard(line);
        cards.push(card);
    });

    cards.forEach((card, i) => {
        const points = getPointsForCard(card);

        for (let j = 1; j <= points; j++) {
            if (cards[i + j]) {
                cards[i + j].totalCount += 1 * card.totalCount;
            }
        }
    });

    let totalCards = 0;
    cards.forEach(card => {
        totalCards += card.totalCount;
    });

    console.log(totalCards);
});