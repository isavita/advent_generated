const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const allergenMap = {};
const ingredientCount = {};
const safeIngredients = {};

for (const line of data) {
    const parts = line.split(' (contains ');
    const ingredients = parts[0].split(' ');
    const allergens = parts.length > 1 ? parts[1].slice(0, -1).split(', ') : [];

    for (const ingredient of ingredients) {
        ingredientCount[ingredient] = (ingredientCount[ingredient] || 0) + 1;
        safeIngredients[ingredient] = true;
    }

    for (const allergen of allergens) {
        if (!allergenMap[allergen]) {
            allergenMap[allergen] = {};
            for (const ingredient of ingredients) {
                allergenMap[allergen][ingredient] = true;
            }
        } else {
            for (const ingredient in allergenMap[allergen]) {
                if (!ingredients.includes(ingredient)) {
                    delete allergenMap[allergen][ingredient];
                }
            }
        }
    }
}

for (const ingredients of Object.values(allergenMap)) {
    for (const ingredient in ingredients) {
        delete safeIngredients[ingredient];
    }
}

let count = 0;
for (const ingredient in safeIngredients) {
    count += ingredientCount[ingredient];
}

console.log(count);