const fs = require('fs');

const instructions = fs.readFileSync('input.txt', 'utf8').split('\n').filter(Boolean);

const getBathroomCode = (instructions) => {
    const keypad = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9]
    ];
    let x = 1, y = 1;
    let code = '';

    for (let instruction of instructions) {
        for (let move of instruction) {
            switch (move) {
                case 'U':
                    if (x > 0) x--;
                    break;
                case 'D':
                    if (x < 2) x++;
                    break;
                case 'L':
                    if (y > 0) y--;
                    break;
                case 'R':
                    if (y < 2) y++;
                    break;
            }
        }
        code += keypad[x][y];
    }

    return code;
};

const code = getBathroomCode(instructions);
console.log(code);