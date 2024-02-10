const fs = require('fs');

class Card {
    constructor() {
        this.winnings = new Map();
        this.givens = new Map();
        this.totalCount = 1;
    }
}

function getPointsForCard(card) {
    let points = 0;
    for (let [given, count] of card.givens) {
        if (card.winnings.has(given)) {
            points += count * card.winnings.get(given);
        }
    }
    return points;
}

function lexLineIntoCard(line) {
    const [_, cardDataStr, __] = line.split(": ");
    const cardData = cardDataStr.split(" | ");
    
    const re = /[0-9]{1,2}/g;
    
    const winnings = new Map();
    cardData[0].match(re).forEach(point => {
        winnings.set(point, (winnings.get(point) || 0) + 1);
    });
    
    const givens = new Map();
    cardData[1].match(re).forEach(point => {
        givens.set(point, (givens.get(point) || 0) + 1);
    });
    
    const card = new Card();
    card.winnings = winnings;
    card.givens = givens;
    
    return card;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error("Error reading file:", err);
        return;
    }
    
    const input = data.trim();
    const lines = input.split("\n");
    
    const cards = [];
    
    lines.forEach(line => {
        if (line.length === 0) {
            return;
        }
        const card = lexLineIntoCard(line);
        cards.push(card);
    });
    
    cards.forEach((card, i) => {
        const points = getPointsForCard(card);
        
        for (let j = 1; j <= points; j++) {
            if (cards[i+j]) {
                cards[i+j].totalCount += 1 * card.totalCount;
            }
        }
    });
    
    let totalCards = 0;
    cards.forEach(card => {
        totalCards += card.totalCount;
    });
    
    console.log(totalCards);
});