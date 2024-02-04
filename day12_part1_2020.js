const fs = require('fs');

class Ship {
    constructor() {
        this.x = 0;
        this.y = 0;
        this.facing = 0;
    }

    processInstruction(action, value) {
        switch (action) {
            case 'N':
                this.y += value;
                break;
            case 'S':
                this.y -= value;
                break;
            case 'E':
                this.x += value;
                break;
            case 'W':
                this.x -= value;
                break;
            case 'L':
                this.facing = (this.facing - value + 360) % 360;
                break;
            case 'R':
                this.facing = (this.facing + value) % 360;
                break;
            case 'F':
                switch (this.facing) {
                    case 0:
                        this.x += value;
                        break;
                    case 90:
                        this.y -= value;
                        break;
                    case 180:
                        this.x -= value;
                        break;
                    case 270:
                        this.y += value;
                        break;
                }
                break;
        }
    }
}

const ship = new Ship();

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
for (let i = 0; i < input.length; i++) {
    const line = input[i];
    const action = line[0];
    const value = parseInt(line.substring(1));
    ship.processInstruction(action, value);
}

const manhattanDistance = Math.abs(ship.x) + Math.abs(ship.y);
console.log(manhattanDistance);