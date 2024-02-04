
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const cubes = {
    red: 12,
    green: 13,
    blue: 14
};

let possibleGames = [];

for (let i = 0; i < input.length; i++) {
    const gameInfo = input[i].split(': ')[1].split('; ');
    let isValid = true;

    for (let j = 0; j < gameInfo.length; j++) {
        const cubesInfo = gameInfo[j].split(', ');

        for (let k = 0; k < cubesInfo.length; k++) {
            const [num, color] = cubesInfo[k].split(' ');
            if (cubes[color] < parseInt(num)) {
                isValid = false;
                break;
            }
        }
    }

    if (isValid) {
        possibleGames.push(i + 1);
    }
}

console.log(possibleGames.reduce((acc, curr) => acc + curr, 0));
