import * as fs from 'fs';

function getMaxCalories(filePath: string): number {
    const data = fs.readFileSync(filePath, 'utf-8').trim();
    const elfCalories = data.split('\n\n').map(elf => 
        elf.split('\n').reduce((sum, calories) => sum + Number(calories), 0)
    );
    return Math.max(...elfCalories);
}

const maxCalories = getMaxCalories('input.txt');
console.log(maxCalories);