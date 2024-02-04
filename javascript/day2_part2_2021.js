const fs = require('fs');

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const lines = data.split('\n');
    let horizontalPosition = 0;
    let depth = 0;
    let aim = 0;

    lines.forEach(line => {
        const command = line.split(' ');
        const direction = command[0];
        const units = parseInt(command[1]);

        switch (direction) {
            case 'forward':
                horizontalPosition += units;
                depth += aim * units;
                break;
            case 'down':
                aim += units;
                break;
            case 'up':
                aim -= units;
                break;
        }
    });

    const product = horizontalPosition * depth;
    console.log(product);
});