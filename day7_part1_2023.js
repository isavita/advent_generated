const fs = require('fs');

const HighCard = 1;
const OnePair = 2;
const TwoPair = 3;
const ThreeKind = 4;
const FullHouse = 5;
const FourKind = 6;
const FiveKind = 7;

let matches = [[], [], [], [], [], [], []];

class Hand {
    constructor(cards, bid) {
        this.cards = cards;
        this.bid = bid;
    }
}

class RankedHand {
    constructor(hand, rank) {
        this.hand = hand;
        this.rank = rank;
    }
}

function findMatches(hands) {
    for (let hand of hands) {
        let count = {};
        for (let card of hand.cards) {
            count[card] = (count[card] || 0) + 1;
        }

        let value = 1;
        for (let c in count) {
            value *= count[c];
        }

        switch (value) {
            case 1:
                matches[6].push(hand);
                break;
            case 2:
                matches[5].push(hand);
                break;
            case 3:
                matches[3].push(hand);
                break;
            case 4:
                if (Object.keys(count).length === 2) {
                    matches[1].push(hand);
                } else {
                    matches[4].push(hand);
                }
                break;
            case 5:
                matches[0].push(hand);
                break;
            case 6:
                matches[2].push(hand);
                break;
            default:
                console.log("oh no");
        }
    }
}

function convertAndOrderMatches() {
    let convertedMatches = [];

    for (let category of matches) {
        let temp = [];

        for (let hand of category) {
            let cards = hand.cards.replace(/A/g, 'E').replace(/T/g, 'A').replace(/J/g, 'B').replace(/Q/g, 'C').replace(/K/g, 'D');
            let num = parseInt(cards, 16);

            temp.push(new RankedHand(hand, num));
        }

        temp.sort((a, b) => b.rank - a.rank);

        convertedMatches.push(...temp);
    }

    return convertedMatches;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const lines = data.split('\n');
    let hands = [];

    const re = /[\dAKQJT]+/;
    const bidRe = / [\d]+/;

    for (let line of lines) {
        if (line.length === 0) {
            continue;
        }

        const cards = line.match(re)[0];
        const bid = parseInt(line.match(bidRe)[0].slice(1));

        hands.push(new Hand(cards, bid));
    }

    findMatches(hands);

    const convertedMatches = convertAndOrderMatches();

    let total = 0;
    for (let i = 0; i < convertedMatches.length; i++) {
        total += convertedMatches[i].hand.bid * (convertedMatches.length - i);
    }

    console.log(total);
});