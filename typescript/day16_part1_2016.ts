const fs = require('fs');

const diskLength = 272; // Disk length for the problem

function readInitialState(filename) {
	const data = fs.readFileSync(filename, 'utf8');
	return data.trim();
}

function generateData(initialState, length) {
	let data = initialState;
	while (data.length < length) {
		let b = '';
		for (let i = data.length - 1; i >= 0; i--) {
			b += data[i] === '0' ? '1' : '0';
		}
		data = data + '0' + b;
	}
	return data.slice(0, length);
}

function calculateChecksum(data) {
	while (data.length % 2 === 0) {
		let b = '';
		for (let i = 0; i < data.length; i += 2) {
			b += data[i] === data[i + 1] ? '1' : '0';
		}
		data = b;
	}
	return data;
}

const initialState = readInitialState('input.txt');
const data = generateData(initialState, diskLength);
const checksum = calculateChecksum(data);
console.log('Checksum:', checksum);