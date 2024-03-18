const fs = require('fs');

const gameRegex = /Game (\d+): (.+)/;
const cubeRegex = /(\d+) (red|green|blue)/g;

let totalPower = 0;

fs.readFileSync('input.txt', 'utf8')
  .split('\n')
  .forEach((line) => {
    const matches = gameRegex.exec(line);
    if (matches) {
      const rounds = matches[2].split(';');
      let maxRed = 0, maxGreen = 0, maxBlue = 0;

      for (const round of rounds) {
        let red = 0, green = 0, blue = 0;
        let cubes;
        while ((cubes = cubeRegex.exec(round)) !== null) {
          const count = parseInt(cubes[1]);
          switch (cubes[2]) {
            case 'red':
              red += count;
              break;
            case 'green':
              green += count;
              break;
            case 'blue':
              blue += count;
              break;
          }
        }

        if (red > maxRed) maxRed = red;
        if (green > maxGreen) maxGreen = green;
        if (blue > maxBlue) maxBlue = blue;
      }

      const power = maxRed * maxGreen * maxBlue;
      totalPower += power;
    }
  });

console.log(totalPower);