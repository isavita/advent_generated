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

    for (const line of lines) {
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
    }

    return ingredients;
}

function calculateScore(amounts: number[], ingredients: Ingredient[]): number {
    let capacity = 0;
    let durability = 0;
    let flavor = 0;
    let texture = 0;

    for (let i = 0; i < amounts.length; i++) {
        capacity += amounts[i] * ingredients[i].capacity;
        durability += amounts[i] * ingredients[i].durability;
        flavor += amounts[i] * ingredients[i].flavor;
        texture += amounts[i] * ingredients[i].texture;
    }

    capacity = Math.max(0, capacity);
    durability = Math.max(0, durability);
    flavor = Math.max(0, flavor);
    texture = Math.max(0, texture);

    return capacity * durability * flavor * texture;
}

function calculateCalories(amounts: number[], ingredients: Ingredient[]): number {
    let calories = 0;
    for (let i = 0; i < amounts.length; i++) {
        calories += amounts[i] * ingredients[i].calories;
    }
    return calories;
}

function findBestScore(ingredients: Ingredient[], targetCalories: number | null = null): number {
    const amounts = new Array(ingredients.length).fill(0);
    let bestScore = 0;

    function recurse(index: number, remaining: number) {
        if (index === ingredients.length - 1) {
            amounts[index] = remaining;
            if (targetCalories === null || calculateCalories(amounts, ingredients) === targetCalories) {
                bestScore = Math.max(bestScore, calculateScore(amounts, ingredients));
            }
            return;
        }

        for (let i = 0; i <= remaining; i++) {
            amounts[index] = i;
            recurse(index + 1, remaining - i);
        }
    }

    recurse(0, 100);
    return bestScore;
}

const input = fs.readFileSync('input.txt', 'utf-8');
const ingredients = parseInput(input);

const bestScorePart1 = findBestScore(ingredients);
console.log(`Part 1: ${bestScorePart1}`);

const bestScorePart2 = findBestScore(ingredients, 500);
console.log(`Part 2: ${bestScorePart2}`);