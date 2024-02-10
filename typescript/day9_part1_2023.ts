const fs = require('fs');

function parseInput(input) {
    const histories = [];
    input.forEach(line => {
        const numbers = parseStringToInts(line);
        histories.push(numbers);
    });
    return histories;
}

function parseStringToInts(numbersLine) {
    const numbers = [];
    const numbersParts = numbersLine.split(' ');
    numbersParts.forEach(numberStr => {
        const number = parseInt(numberStr);
        numbers.push(number);
    });
    return numbers;
}

function allZeros(nums) {
    for (let num of nums) {
        if (num !== 0) {
            return false;
        }
    }
    return true;
}

function calculateExtrapolation(history) {
    const extrapolations = [];
    for (let i = 1; i < history.length; i++) {
        const extrapolation = history[i] - history[i - 1];
        extrapolations.push(extrapolation);
    }
    return extrapolations;
}

function calculateExtrapolations(history) {
    const extrapolationsSeries = [];
    extrapolationsSeries.push(history);

    for (let i = 1; i < history.length; i++) {
        const previousExtrapolations = extrapolationsSeries[i - 1];
        if (allZeros(previousExtrapolations)) {
            return extrapolationsSeries;
        }

        const extrapolations = calculateExtrapolation(previousExtrapolations);
        extrapolationsSeries.push(extrapolations);
    }

    return extrapolationsSeries;
}

function solve(input) {
    const histories = parseInput(input);
    let res = 0;

    histories.forEach(history => {
        const extrapolationsSeries = calculateExtrapolations(history);

        let futurePrediction = 0;
        for (let i = extrapolationsSeries.length - 1; i > -1; i--) {
            futurePrediction = extrapolationsSeries[i][extrapolationsSeries[i].length - 1] + futurePrediction;
        }

        res += futurePrediction;
    });

    return res;
}

function readFile(fileName) {
    const file = fs.readFileSync(fileName, 'utf8');
    return file.trim().split('\n');
}

const input = readFile("input.txt");
console.log(solve(input));