import * as fs from 'fs';

const HighCard = 1, OnePair = 2, TwoPair = 3, ThreeKind = 4, FullHouse = 5, FourKind = 6, FiveKind = 7;

interface Hand {
    cards: string;
    bid: number;
}

interface RankedHand {
    hand: Hand;
    rank: number;
}

let matches: Hand[][] = Array.from({ length: 7 }, () => []);

function findMatches(hands: Hand[]) {
    hands.forEach(hand => {
        const count = new Map<string, number>();

        for (const card of hand.cards) {
            count.set(card, (count.get(card) || 0) + 1);
        }

        let value = 1;
        for (const c of count.values()) {
            value *= c;
        }

        switch (value) {
            case 1: matches[6].push(hand); break;
            case 2: matches[5].push(hand); break;
            case 3: matches[3].push(hand); break;
            case 4: matches[count.size === 2 ? 1 : 4].push(hand); break;
            case 5: matches[0].push(hand); break;
            case 6: matches[2].push(hand); break;
            default: console.log("oh no");
        }
    });
}

function convertAndOrderMatches(): RankedHand[] {
    let convertedMatches: RankedHand[] = [];

    matches.forEach(category => {
        let temp: RankedHand[] = [];

        category.forEach(hand => {
            let cards = hand.cards.replace(/A/g, 'E').replace(/T/g, 'A').replace(/J/g, 'B').replace(/Q/g, 'C').replace(/K/g, 'D');
            let num = parseInt(cards, 16);

            temp.push({ hand, rank: num });
        });

        temp.sort((a, b) => b.rank - a.rank);
        convertedMatches.push(...temp);
    });

    return convertedMatches;
}

const file = fs.readFileSync('input.txt', 'utf8');
const lines = file.split('\n');
let hands: Hand[] = [];

lines.forEach(line => {
    if (line.length === 0) return;

    const [cards, bid] = line.split(' ');
    hands.push({ cards, bid: parseInt(bid) });
});

findMatches(hands);

const convertedMatches = convertAndOrderMatches();

let total = 0;
for (let i = 0; i < convertedMatches.length; i++) {
    total += convertedMatches[i].hand.bid * (convertedMatches.length - i);
}

console.log(total);