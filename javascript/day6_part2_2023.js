const fs = require('fs');

function calculateWaysToWinLongRace(time, record) {
	let waysToWin = 0;
	for (let holdTime = 1; holdTime < time; holdTime++) {
		let travelTime = time - holdTime;
		let distance = holdTime * travelTime;
		if (distance > record) {
			waysToWin++;
		}
	}
	return waysToWin;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
	if (err) {
		console.error("Error reading file:", err);
		return;
	}

	const lines = data.split('\n');
	let time = 0;
	let distance = 0;

	for (let i = 0; i < lines.length; i++) {
		const parts = lines[i].split(':');
		const line = parts[1].replace(/\s/g, ''); // Remove spaces
		if (time === 0) {
			time = parseInt(line);
		} else {
			distance = parseInt(line);
		}
	}

	const waysToWin = calculateWaysToWinLongRace(time, distance);

	console.log(waysToWin);
});