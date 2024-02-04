const fs = require('fs');

const valueDict = {'J': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, '8': 8, '9': 9, 'T': 10, 'Q': 11, 'K': 12, 'A': 13};

class Hand {
	constructor(cards, bid) {
		this.cards = cards;
		this.bid = bid;
	}
}

fs.readFile('input.txt', 'utf8', (err, data) => {
	if (err) {
		console.error(err);
		return;
	}

	const lines = data.split('\n');
	const hands = [];

	const re = /[\dAKQJT]+/;
	const bidRe = / [\d]+/;

	for (const line of lines) {
		if (line.length === 0) {
			continue;
		}

		const cards = line.match(re)[0];
		const bid = parseInt(line.match(bidRe)[0].slice(1));

		hands.push(new Hand(cards, bid));
	}

	const matches = [[], [], [], [], [], [], []];

	for (const hand of hands) {
		const count = {};

		for (const i of hand.cards) {
			count[i] = (count[i] || 0) + 1;
		}

		if (count['J'] > 0) {
			let highV = 0;
			let highKey = 'J';
			for (const y in count) {
				if (y !== 'J') {
					if (count[y] > highV) {
						highKey = y;
						highV = count[y];
					} else if (count[y] === highV && valueDict[y] > valueDict[highKey]) {
						highKey = y;
					}
				}
			}
			if (highKey !== 'J') {
				count[highKey] += count['J'];
				delete count['J'];
			}
		}

		let value = 1;
		for (const i in count) {
			value *= count[i];
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

	const convertedMatches = [];

	for (const x of matches) {
		const temp = [];
		for (const i of x) {
			let y = i.cards.replace(/A/g, 'E').replace(/T/g, 'A').replace(/J/g, '1').replace(/Q/g, 'C').replace(/K/g, 'D');
			const val = parseInt(y, 16);
			temp.push([val, i.bid]);
		}
		temp.sort((a, b) => b[0] - a[0]);
		convertedMatches.push(...temp);
	}

	let total = 0;
	for (let x = 0; x < convertedMatches.length; x++) {
		total += convertedMatches[x][1] * (convertedMatches.length - x);
	}

	console.log(total);
});