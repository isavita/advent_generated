import * as fs from 'fs';

interface Ingredient {
    name: string;
    capacity: number;
    durability: number;
    flavor: number;
    texture: number;
    calories: number;
}

function parseInput(input: string): Ingredient[] {
    const lines = input.trim().split('\n');
    const ingredients: Ingredient[] = [];

    lines.forEach(line => {
        const match = line.match(/(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)/);
        if (match) {
            ingredients.push({
                name: match[1],
                capacity: parseInt(match[2], 10),
                durability: parseInt(match[3], 10),
                flavor: parseInt(match[4], 10),
                texture: parseInt(match[5], 10),
                calories: parseInt(match[6], 10),
            });
        }
    });

    return ingredients;
}

function calculateScore(ingredients: Ingredient[], amounts: number[]): number {
    let capacity = 0;
    let durability = 0;
    let flavor = 0;
    let texture = 0;

    for (let i = 0; i < ingredients.length; i++) {
        capacity += ingredients[i].capacity * amounts[i];
        durability += ingredients[i].durability * amounts[i];
        flavor += ingredients[i].flavor * amounts[i];
        texture += ingredients[i].texture * amounts[i];
    }

    if (capacity < 0 || durability < 0 || flavor < 0 || texture < 0) {
        return 0;
    }

    return capacity * durability * flavor * texture;
}

function findBestScore(ingredients: Ingredient[]): number {
    let bestScore = 0;

    const totalTeaspoons = 100;
    const numIngredients = ingredients.length;

    for (let i = 0; i <= totalTeaspoons; i++) {
        for (let j = 0; j <= totalTeaspoons - i; j++) {
            for (let k = 0; k <= totalTeaspoons - i - j; k++) {
                const l = totalTeaspoons - i - j - k;
                if (numIngredients === 4) {
                    const amounts = [i, j, k, l];
                    const score = calculateScore(ingredients, amounts);
                    if (score > bestScore) {
                        bestScore = score;
                    }
                } else if (numIngredients === 3) {
                    const amounts = [i, j, k];
                    const score = calculateScore(ingredients, amounts);
                    if (score > bestScore) {
                        bestScore = score;
                    }
                } else if (numIngredients === 2) {
                    const amounts = [i, j];
                    const score = calculateScore(ingredients, amounts);
                    if (score > bestScore) {
                        bestScore = score;
                    }
                }
            }
        }
    }

    return bestScore;
}

const input = fs.readFileSync('input.txt', 'utf8');
const ingredients = parseInput(input);
const bestScore = findBestScore(ingredients);

console.log(bestScore);