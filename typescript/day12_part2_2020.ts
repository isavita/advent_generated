import * as fs from 'fs';

class Ship {
  x = 0;
  y = 0;
  waypointX = 10;
  waypointY = 1;

  processInstruction(action: string, value: number): void {
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

  rotateWaypoint(degrees: number): void {
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

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const ship = new Ship();

input.forEach((line: string) => {
  const action = line[0];
  const value = parseInt(line.slice(1));
  ship.processInstruction(action, value);
});

console.log(Math.abs(ship.x) + Math.abs(ship.y));