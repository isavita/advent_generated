const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const head = [0, 0];
const tail = [0, 0];
const visited = { '0,0': true };

const abs = (x) => x < 0 ? -x : x;

for (const line of input) {
    const [dir, steps] = line.split(' ');
    const numSteps = parseInt(steps);

    for (let i = 0; i < numSteps; i++) {
        switch (dir) {
            case 'R':
                head[0]++;
                break;
            case 'L':
                head[0]--;
                break;
            case 'U':
                head[1]++;
                break;
            case 'D':
                head[1]--;
                break;
        }

        if (abs(head[0] - tail[0]) > 1 || abs(head[1] - tail[1]) > 1) {
            if (head[0] !== tail[0] && head[1] !== tail[1]) {
                if (head[0] > tail[0]) {
                    tail[0]++;
                } else {
                    tail[0]--;
                }
                if (head[1] > tail[1]) {
                    tail[1]++;
                } else {
                    tail[1]--;
                }
            } else {
                if (head[0] > tail[0]) {
                    tail[0]++;
                } else if (head[0] < tail[0]) {
                    tail[0]--;
                }
                if (head[1] > tail[1]) {
                    tail[1]++;
                } else if (head[1] < tail[1]) {
                    tail[1]--;
                }
            }
        }

        visited[`${tail[0]},${tail[1]}`] = true;
    }
}

console.log(Object.keys(visited).length);