const fs = require('fs');

class Ship {
    constructor() {
        this.x = 0;
        this.y = 0;
        this.waypointX = 10;
        this.waypointY = 1;
    }

    processInstruction(action, value) {
        switch (action) {
            case 'N':
                this.waypointY += value;
                break;
            case 'S':
                this.waypointY -= value;
                break;
            case 'E':
                this.waypointX += value;
                break;
            case 'W':
                this.waypointX -= value;
                break;
            case 'L':
                this.rotateWaypoint(-value);
                break;
            case 'R':
                this.rotateWaypoint(value);
                break;
            case 'F':
                this.x += this.waypointX * value;
                this.y += this.waypointY * value;
                break;
        }
    }

    rotateWaypoint(degrees) {
        degrees = (degrees + 360) % 360;
        switch (degrees) {
            case 90:
            case -270:
                [this.waypointX, this.waypointY] = [this.waypointY, -this.waypointX];
                break;
            case 180:
            case -180:
                [this.waypointX, this.waypointY] = [-this.waypointX, -this.waypointY];
                break;
            case 270:
            case -90:
                [this.waypointX, this.waypointY] = [-this.waypointY, this.waypointX];
                break;
        }
    }
}

const ship = new Ship();

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
for (let line of input) {
    const action = line[0];
    const value = parseInt(line.slice(1));
    ship.processInstruction(action, value);
}

const manhattanDistance = Math.abs(ship.x) + Math.abs(ship.y);
console.log(manhattanDistance);