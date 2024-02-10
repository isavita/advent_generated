const fs = require('fs');

class Ingredient {
    constructor(name, capacity, durability, flavor, texture) {
        this.name = name;
        this.capacity = capacity;
        this.durability = durability;
        this.flavor = flavor;
        this.texture = texture;
    }
}

function readIngredients(filename) {
    const data = fs.readFileSync(filename, 'utf8').split('\n');
    const ingredients = [];

    for (let i = 0; i < data.length; i++) {
        const parts = data[i].split(' ');
        if (parts.length < 11) {
            continue;
        }

        const capacity = parseInt(parts[2].slice(0, -1));
        const durability = parseInt(parts[4].slice(0, -1));
        const flavor = parseInt(parts[6].slice(0, -1));
        const texture = parseInt(parts[8].slice(0, -1));

        ingredients.push(new Ingredient(parts[0], capacity, durability, flavor, texture));
    }

    return ingredients;
}

function findMaxScore(ingredients, totalTeaspoons) {
    return calculateMaxScore(ingredients, 0, totalTeaspoons, []);
}

function calculateMaxScore(ingredients, index, remaining, teaspoons) {
    if (index === ingredients.length - 1) {
        teaspoons.push(remaining);
        return score(ingredients, teaspoons);
    }

    let maxScore = 0;
    for (let i = 0; i <= remaining; i++) {
        const score = calculateMaxScore(ingredients, index + 1, remaining - i, [...teaspoons, i]);
        if (score > maxScore) {
            maxScore = score;
        }
    }
    return maxScore;
}

function score(ingredients, teaspoons) {
    let capacity = 0;
    let durability = 0;
    let flavor = 0;
    let texture = 0;

    for (let i = 0; i < ingredients.length; i++) {
        capacity += ingredients[i].capacity * teaspoons[i];
        durability += ingredients[i].durability * teaspoons[i];
        flavor += ingredients[i].flavor * teaspoons[i];
        texture += ingredients[i].texture * teaspoons[i];
    }

    capacity = capacity < 0 ? 0 : capacity;
    durability = durability < 0 ? 0 : durability;
    flavor = flavor < 0 ? 0 : flavor;
    texture = texture < 0 ? 0 : texture;

    return capacity * durability * flavor * texture;
}

const ingredients = readIngredients('input.txt');
const maxScore = findMaxScore(ingredients, 100);
console.log(maxScore);