import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8');
const elvesCalories = input.split('\n\n').map(group => 
    group.split('\n').reduce((sum, line) => sum + (parseInt(line) || 0), 0)
);

const topThreeCalories = elvesCalories.sort((a, b) => b - a).slice(0, 3);
const totalCalories = topThreeCalories.reduce((sum, calories) => sum + calories, 0);

console.log(totalCalories);