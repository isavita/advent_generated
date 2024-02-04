const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const allergenMap = {};
const ingredientAllergen = {};

for (const line of input) {
  const [ingredients, allergens] = line.split(' (contains ');
  const ingredientList = ingredients.split(' ');
  const allergenList = allergens.slice(0, -1).split(', ');

  for (const allergen of allergenList) {
    if (!allergenMap[allergen]) {
      allergenMap[allergen] = {};
      for (const ingredient of ingredientList) {
        allergenMap[allergen][ingredient] = true;
      }
    } else {
      for (const ingredient in allergenMap[allergen]) {
        if (!ingredientList.includes(ingredient)) {
          delete allergenMap[allergen][ingredient];
        }
      }
    }
  }
}

while (Object.keys(allergenMap).length > 0) {
  for (const allergen in allergenMap) {
    if (Object.keys(allergenMap[allergen]).length === 1) {
      const ingredient = Object.keys(allergenMap[allergen])[0];
      ingredientAllergen[allergen] = ingredient;
      for (const key in allergenMap) {
        delete allergenMap[key][ingredient];
      }
      delete allergenMap[allergen];
    }
  }
}

const allergens = Object.keys(ingredientAllergen).sort();
const result = allergens.map(allergen => ingredientAllergen[allergen]);

console.log(result.join(','));