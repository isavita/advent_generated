const fs = require('fs');
const path = require('path');

class UnstableDiffusion {
    constructor(inputPath) {
        this.elves = new Set();
        this.directions = ['N', 'S', 'W', 'E'];
        this.readInput(inputPath);
    }

    readInput(inputPath) {
        const data = fs.readFileSync(path.resolve(__dirname, inputPath), 'utf-8');
        const lines = data.trim().split('\n');
        lines.forEach((line, y) => {
            for (let x = 0; x < line.length; x++) {
                if (line[x] === '#') {
                    this.elves.add(`${x},${y}`);
                }
            }
        });
    }

    getAdjacency(x, y) {
        return [
            [x, y - 1],     // N
            [x + 1, y - 1], // NE
            [x + 1, y],     // E
            [x + 1, y + 1], // SE
            [x, y + 1],     // S
            [x - 1, y + 1], // SW
            [x - 1, y],     // W
            [x - 1, y - 1], // NW
        ];
    }

    simulateRounds(rounds) {
        for (let round = 0; round < rounds; round++) {
            const proposals = new Map();
            const elfMoves = new Map();

            // First half: Propose moves
            this.elves.forEach(elf => {
                const [x, y] = elf.split(',').map(Number);
                const adjacent = this.getAdjacency(x, y);
                const hasAdjacent = adjacent.some(pos => this.elves.has(`${pos[0]},${pos[1]}`));

                if (!hasAdjacent) return;

                for (let dir of this.directions) {
                    let canMove = false;
                    let newPos = null;

                    switch (dir) {
                        case 'N':
                            canMove = !this.elves.has(`${x},${y - 1}`) &&
                                      !this.elves.has(`${x + 1},${y - 1}`) &&
                                      !this.elves.has(`${x - 1},${y - 1}`);
                            if (canMove) newPos = `${x},${y - 1}`;
                            break;
                        case 'S':
                            canMove = !this.elves.has(`${x},${y + 1}`) &&
                                      !this.elves.has(`${x + 1},${y + 1}`) &&
                                      !this.elves.has(`${x - 1},${y + 1}`);
                            if (canMove) newPos = `${x},${y + 1}`;
                            break;
                        case 'W':
                            canMove = !this.elves.has(`${x - 1},${y}`) &&
                                      !this.elves.has(`${x - 1},${y + 1}`) &&
                                      !this.elves.has(`${x - 1},${y - 1}`);
                            if (canMove) newPos = `${x - 1},${y}`;
                            break;
                        case 'E':
                            canMove = !this.elves.has(`${x + 1},${y}`) &&
                                      !this.elves.has(`${x + 1},${y + 1}`) &&
                                      !this.elves.has(`${x + 1},${y - 1}`);
                            if (canMove) newPos = `${x + 1},${y}`;
                            break;
                    }

                    if (newPos) {
                        if (!proposals.has(newPos)) {
                            proposals.set(newPos, []);
                        }
                        proposals.get(newPos).push(elf);
                        elfMoves.set(elf, newPos);
                        break;
                    }
                }
            });

            // Second half: Execute moves
            elfMoves.forEach((newPos, elf) => {
                if (proposals.get(newPos).length === 1) {
                    this.elves.delete(elf);
                    this.elves.add(newPos);
                }
            });

            // Rotate directions
            this.directions.push(this.directions.shift());
        }
    }

    calculateEmptyGround() {
        let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
        this.elves.forEach(elf => {
            const [x, y] = elf.split(',').map(Number);
            if (x < minX) minX = x;
            if (x > maxX) maxX = x;
            if (y < minY) minY = y;
            if (y > maxY) maxY = y;
        });

        const width = maxX - minX + 1;
        const height = maxY - minY + 1;
        const area = width * height;
        const emptyGround = area - this.elves.size;
        return emptyGround;
    }

    run(inputPath, rounds) {
        this.simulateRounds(rounds);
        const emptyTiles = this.calculateEmptyGround();
        console.log(emptyTiles);
    }
}

const diffusion = new UnstableDiffusion('input.txt');
diffusion.run('input.txt', 10);
