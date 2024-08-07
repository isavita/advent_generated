import * as fs from 'fs';

class Scrambler {
    pw: string[];

    constructor(pw: string) {
        this.pw = pw.split('');
    }

    toString(): string {
        return this.pw.join('');
    }

    swapPositions(x: number, y: number): void {
        [this.pw[x], this.pw[y]] = [this.pw[y], this.pw[x]];
    }

    swapLetters(x: string, y: string): void {
        this.swapPositions(this.pw.indexOf(x), this.pw.indexOf(y));
    }

    rotate(steps: number): void {
        const length = this.pw.length;
        steps = steps % length;
        if (steps < 0) {
            steps += length;
        }
        this.pw = this.pw.slice(length - steps).concat(this.pw.slice(0, length - steps));
    }

    rotateLetter(x: string): void {
        let index = this.pw.indexOf(x);
        if (index >= 4) {
            index++;
        }
        this.rotate(index + 1);
    }

    derotateLetter(x: string): void {
        let index = this.pw.indexOf(x);
        let rot: number;
        if (index % 2 === 1) {
            rot = -(index + 1) / 2;
        } else if (index !== 0) {
            rot = (6 - index) / 2;
        } else {
            rot = -1;
        }
        this.rotate(rot);
    }

    reverse(x: number, y: number): void {
        while (x < y) {
            [this.pw[x], this.pw[y]] = [this.pw[y], this.pw[x]];
            x++;
            y--;
        }
    }

    move(x: number, y: number): void {
        const ch = this.pw[x];
        if (x < y) {
            this.pw.copyWithin(x, x + 1, y + 1);
        } else {
            this.pw.copyWithin(y + 1, y, x);
        }
        this.pw[y] = ch;
    }

    scramble(instructions: string[], direction: number): Scrambler {
        if (direction < 0) {
            instructions.reverse();
        }
        instructions.forEach(instruction => {
            const line = instruction.split(' ');
            if (line[0] === 'swap') {
                const x = line[2], y = line[line.length - 1];
                if (line[1] === 'position') {
                    this.swapPositions(parseInt(x), parseInt(y));
                } else {
                    this.swapLetters(x, y);
                }
            } else if (line[0] === 'rotate') {
                if (line[1] === 'based') {
                    if (direction > 0) {
                        this.rotateLetter(line[line.length - 1]);
                    } else {
                        this.derotateLetter(line[line.length - 1]);
                    }
                } else {
                    let x = parseInt(line[2]);
                    if (line[1] === 'left') {
                        x = -x;
                    }
                    if (direction < 0) {
                        x = -x;
                    }
                    this.rotate(x);
                }
            } else if (line[0] === 'reverse') {
                this.reverse(parseInt(line[2]), parseInt(line[line.length - 1]));
            } else if (line[0] === 'move') {
                let x = parseInt(line[2]), y = parseInt(line[line.length - 1]);
                if (direction < 0) {
                    [x, y] = [y, x];
                }
                this.move(x, y);
            }
        });
        return this;
    }

    unscramble(instructions: string[]): Scrambler {
        return this.scramble(instructions, -1);
    }
}

const instructions = fs.readFileSync('input.txt', 'utf-8').split('\n');
const hashed = "fbgdceah";
const scrambler = new Scrambler(hashed);
const result = scrambler.unscramble(instructions);
console.log(result.toString());