const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const distances = {};

input.forEach(line => {
    const [cities, distance] = line.split(' = ');
    const [cityA, cityB] = cities.split(' to ');

    if (!distances[cityA]) distances[cityA] = {};
    if (!distances[cityB]) distances[cityB] = {};

    distances[cityA][cityB] = parseInt(distance);
    distances[cityB][cityA] = parseInt(distance);
});

const cities = Object.keys(distances);

let minDistance = Infinity;

function permute(arr, start, end) {
    if (start === end) {
        let distance = 0;
        for (let i = 1; i < arr.length; i++) {
            distance += distances[arr[i - 1]][arr[i]];
        }
        minDistance = Math.min(minDistance, distance);
    } else {
        for (let i = start; i <= end; i++) {
            [arr[start], arr[i]] = [arr[i], arr[start]];
            permute(arr, start + 1, end);
            [arr[start], arr[i]] = [arr[i], arr[start]];
        }
    }
}

permute(cities, 0, cities.length - 1);

console.log(minDistance);