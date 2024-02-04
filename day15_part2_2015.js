const fs = require('fs');

class Ingredient {
    constructor(name, capacity, durability, flavor, texture, calories) {
        this.name = name;
        this.capacity = capacity;
        this.durability = durability;
        this.flavor = flavor;
        this.texture = texture;
        this.calories = calories;
    }
}

function readIngredients(filename) {
    const data = fs.readFileSync(filename, 'utf8').split('\n');
    const ingredients = [];

    for (let line of data) {
        const parts = line.split(' ');
        if (parts.length < 11) {
            continue;
        }

        const capacity = parseInt(parts[2].slice(0, -1));
        const durability = parseInt(parts[4].slice(0, -1));
        const flavor = parseInt(parts[6].slice(0, -1));
        const texture = parseInt(parts[8].slice(0, -1));
        const calories = parseInt(parts[10]);

        ingredients.push(new Ingredient(parts[0], capacity, durability, flavor, texture, calories));
    }

    return ingredients;
}

function findMaxScore(ingredients, totalTeaspoons, targetCalories) {
    return calculateMaxScore(ingredients, 0, totalTeaspoons, [], targetCalories);
}

function calculateMaxScore(ingredients, index, remaining, teaspoons, targetCalories) {
    if (index === ingredients.length - 1) {
        teaspoons.push(remaining);
        if (calculateCalories(ingredients, teaspoons) === targetCalories) {
            return score(ingredients, teaspoons);
        }
        return 0;
    }

    let maxScore = 0;
    for (let i = 0; i <= remaining; i++) {
        const score = calculateMaxScore(ingredients, index + 1, remaining - i, [...teaspoons, i], targetCalories);
        if (score > maxScore) {
            maxScore = score;
        }
    }
    return maxScore;
}

function score(ingredients, teaspoons) {
    let capacity = 0, durability = 0, flavor = 0, texture = 0;
    for (let i = 0; i < ingredients.length; i++) {
        capacity += ingredients[i].capacity * teaspoons[i];
        durability += ingredients[i].durability * teaspoons[i];
        flavor += ingredients[i].flavor * teaspoons[i];
        texture += ingredients[i].texture * teaspoons[i];
    }

    capacity = Math.max(0, capacity);
    durability = Math.max(0, durability);
    flavor = Math.max(0, flavor);
    texture = Math.max(0, texture);

    return capacity * durability * flavor * texture;
}

function calculateCalories(ingredients, teaspoons) {
    let calories = 0;
    for (let i = 0; i < ingredients.length; i++) {
        calories += ingredients[i].calories * teaspoons[i];
    }
    return calories;
}

const ingredients = readIngredients('input.txt');
const maxScore = findMaxScore(ingredients, 100, 500);
console.log(maxScore);