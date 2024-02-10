
const fs = require('fs');

const file = fs.readFileSync('input.txt', 'utf-8');
const regex = /Game (\d+): (.+)/g;
const cubeRegex = /(\d+) (red|green|blue)/g;
let totalPower = 0;

let match;
while ((match = regex.exec(file)) !== null) {
    const rounds = match[2].split(';');
    let maxRed = 0, maxGreen = 0, maxBlue = 0;

    rounds.forEach(round => {
        const cubes = round.match(cubeRegex);
        let red = 0, green = 0, blue = 0;

        cubes.forEach(cube => {
            const [count, color] = cube.split(' ');
            switch (color) {
                case 'red':
                    red += parseInt(count);
                    break;
                case 'green':
                    green += parseInt(count);
                    break;
                case 'blue':
                    blue += parseInt(count);
                    break;
            }
        });

        if (red > maxRed) {
            maxRed = red;
        }
        if (green > maxGreen) {
            maxGreen = green;
        }
        if (blue > maxBlue) {
            maxBlue = blue;
        }
    });

    const power = maxRed * maxGreen * maxBlue;
    totalPower += power;
}

console.log(totalPower);
