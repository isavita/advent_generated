import * as fs from 'fs';

const inputFilePath = 'input.txt';
const data = fs.readFileSync(inputFilePath, 'utf-8').trim().split('\n');

const maxCubes = { red: 12, green: 13, blue: 14 };

const parseGame = (line: string) => {
    const [id, sets] = line.split(': ');
    const cubeSets = sets.split('; ').map(set => {
        const cubes: { [key: string]: number } = {};
        set.split(', ').forEach(cube => {
            const [count, color] = cube.split(' ');
            cubes[color] = (cubes[color] || 0) + Number(count);
        });
        return cubes;
    });
    return { id: Number(id.split(' ')[1]), cubeSets };
};

const games = data.map(parseGame);

const validGameIds = games.filter(game => 
    game.cubeSets.every(set => 
        (set.red || 0) <= maxCubes.red && 
        (set.green || 0) <= maxCubes.green && 
        (set.blue || 0) <= maxCubes.blue
    )
).map(game => game.id);

const sumValidGameIds = validGameIds.reduce((sum, id) => sum + id, 0);
console.log(`Sum of valid game IDs: ${sumValidGameIds}`);

const minCubesPower = games.map(game => {
    const minCubes = { red: 0, green: 0, blue: 0 };
    game.cubeSets.forEach(set => {
        minCubes.red = Math.max(minCubes.red, set.red || 0);
        minCubes.green = Math.max(minCubes.green, set.green || 0);
        minCubes.blue = Math.max(minCubes.blue, set.blue || 0);
    });
    return minCubes.red * minCubes.green * minCubes.blue;
});

const totalPower = minCubesPower.reduce((sum, power) => sum + power, 0);
console.log(`Total power of minimum sets: ${totalPower}`);