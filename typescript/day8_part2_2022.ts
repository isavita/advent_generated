const fs = require('fs');

const Neighbors4 = [{ x: 0, y: 1 }, { x: 0, y: -1 }, { x: 1, y: 0 }, { x: -1, y: 0 }];

function main() {
    const grid = {};
    const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
    let y = 0;

    for (const line of input) {
        for (let x = 0; x < line.length; x++) {
            grid[`${x},${y}`] = parseInt(line[x]);
        }
        y++;
    }

    let maxScore = 0;
    for (const point in grid) {
        const p = point.split(',').map(Number);
        const [px, py] = p;
        let score = 1;

        for (const n of Neighbors4) {
            let [nx, ny] = [px, py];
            let view = 0;

            while (true) {
                nx += n.x;
                ny += n.y;

                if (grid[`${nx},${ny}`] !== undefined) {
                    view++;
                    if (grid[`${nx},${ny}`] >= grid[`${px},${py}`]) {
                        score *= view;
                        break;
                    }
                } else {
                    score *= view;
                    break;
                }
            }
        }

        if (score > maxScore) {
            maxScore = score;
        }
    }

    console.log(maxScore);
}

main();