const fs = require('fs');

class Scrambler {
    constructor(pw) {
        this.pw = pw.split('');
    }

    swapPositions(x, y) {
        [this.pw[x], this.pw[y]] = [this.pw[y], this.pw[x]];
    }

    swapLetters(x, y) {
        this.swapPositions(this.pw.indexOf(x), this.pw.indexOf(y));
    }

    rotate(steps) {
        const length = this.pw.length;
        steps = steps % length;
        if (steps < 0) {
            steps += length;
        }
        this.pw = [...this.pw.slice(length - steps), ...this.pw.slice(0, length - steps)];
    }

    rotateLetter(x) {
        const index = this.pw.indexOf(x);
        const rot = index >= 4 ? index + 2 : index + 1;
        this.rotate(rot);
    }

    derotateLetter(x) {
        const index = this.pw.indexOf(x);
        let rot;
        if (index % 2 === 1) {
            rot = -(index + 1) / 2;
        } else if (index !== 0) {
            rot = (6 - index) / 2;
        } else {
            rot = -1;
        }
        this.rotate(rot);
    }

    reverse(x, y) {
        while (x < y) {
            [this.pw[x], this.pw[y]] = [this.pw[y], this.pw[x]];
            x++;
            y--;
        }
    }

    move(x, y) {
        const ch = this.pw[x];
        if (x < y) {
            this.pw.splice(y, 0, this.pw.splice(x, 1)[0]);
        } else {
            this.pw.splice(y, 0, this.pw.splice(x, 1)[0]);
        }
    }

    scramble(instructions, direction) {
        if (direction < 0) {
            instructions = instructions.reverse();
        }

        instructions.forEach(instruction => {
            const line = instruction.split(' ');
            switch (true) {
                case instruction.startsWith("swap"):
                    if (line[1] === "position") {
                        this.swapPositions(parseInt(line[2]), parseInt(line[line.length - 1]));
                    } else {
                        this.swapLetters(line[2], line[line.length - 1]);
                    }
                    break;
                case instruction.startsWith("rotate"):
                    if (line[1] === "based") {
                        if (direction > 0) {
                            this.rotateLetter(line[line.length - 1]);
                        } else {
                            this.derotateLetter(line[line.length - 1]);
                        }
                    } else {
                        let steps = parseInt(line[2]);
                        if (line[1] === "left") {
                            steps = -steps;
                        }
                        if (direction < 0) {
                            steps = -steps;
                        }
                        this.rotate(steps);
                    }
                    break;
                case instruction.startsWith("reverse"):
                    this.reverse(parseInt(line[2]), parseInt(line[line.length - 1]));
                    break;
                case instruction.startsWith("move"):
                    let x = parseInt(line[2]);
                    let y = parseInt(line[line.length - 1]);
                    if (direction < 0) {
                        [x, y] = [y, x];
                    }
                    this.move(x, y);
                    break;
            }
        });
    }

    unscramble(instructions) {
        this.scramble(instructions, -1);
    }

    toString() {
        return this.pw.join('');
    }
}

const input = fs.readFileSync("input.txt", "utf8");
const lines = input.split('\n').filter(Boolean);

const hashed = "fbgdceah";
const scrambler = new Scrambler(hashed);
scrambler.unscramble(lines);
console.log(scrambler.toString());