const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const distances = {};

input.forEach((line) => {
    const [cities, dist] = line.split(' = ');
    const [city1, city2] = cities.split(' to ');
    
    if (!distances[city1]) {
        distances[city1] = {};
    }
    if (!distances[city2]) {
        distances[city2] = {};
    }
    
    distances[city1][city2] = parseInt(dist);
    distances[city2][city1] = parseInt(dist);
});

const cities = Object.keys(distances);
let shortest = Infinity;
let longest = 0;

function permute(permutation) {
    let distance = 0;
    for (let i = 0; i < permutation.length - 1; i++) {
        distance += distances[permutation[i]][permutation[i + 1]];
    }
    
    shortest = Math.min(shortest, distance);
    longest = Math.max(longest, distance);
}

function generatePermutations(permutation, n) {
    if (n === 1) {
        permute(permutation);
    } else {
        for (let i = 0; i < n - 1; i++) {
            generatePermutations(permutation, n - 1);
            if (n % 2 === 0) {
                [permutation[i], permutation[n - 1]] = [permutation[n - 1], permutation[i]];
            } else {
                [permutation[0], permutation[n - 1]] = [permutation[n - 1], permutation[0]];
            }
        }
        generatePermutations(permutation, n - 1);
    }
}

generatePermutations(cities, cities.length);

console.log(shortest);
console.log(longest);