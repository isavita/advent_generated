import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

const calculateGammaEpsilon = (data: string[]) => {
    const length = data[0].length;
    const count = Array(length).fill(0);

    for (const number of data) {
        for (let i = 0; i < length; i++) {
            count[i] += number[i] === '1' ? 1 : -1;
        }
    }

    const gamma = count.map(c => (c > 0 ? '1' : '0')).join('');
    const epsilon = count.map(c => (c < 0 ? '1' : '0')).join('');
    
    return [parseInt(gamma, 2), parseInt(epsilon, 2)];
};

const calculateRating = (data: string[], isOxygen: boolean) => {
    let filtered = [...data];
    const length = filtered[0].length;

    for (let i = 0; i < length && filtered.length > 1; i++) {
        const count = filtered.reduce((acc, number) => acc + (number[i] === '1' ? 1 : -1), 0);
        const keepBit = isOxygen ? (count >= 0 ? '1' : '0') : (count < 0 ? '1' : '0');
        filtered = filtered.filter(number => number[i] === keepBit);
    }

    return parseInt(filtered[0], 2);
};

const [gamma, epsilon] = calculateGammaEpsilon(input);
const powerConsumption = gamma * epsilon;

const oxygenRating = calculateRating(input, true);
const co2Rating = calculateRating(input, false);
const lifeSupportRating = oxygenRating * co2Rating;

console.log(`Power Consumption: ${powerConsumption}`);
console.log(`Life Support Rating: ${lifeSupportRating}`);