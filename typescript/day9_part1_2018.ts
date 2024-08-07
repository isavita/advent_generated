import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim();
const [playerCount, lastMarble] = input.match(/\d+/g)!.map(Number);

class MarbleGame {
    private circle: number[] = [0];
    private currentIndex: number = 0;
    private scores: number[] = Array(playerCount).fill(0);

    play() {
        for (let marble = 1; marble <= lastMarble; marble++) {
            const currentPlayer = (marble - 1) % playerCount;
            if (marble % 23 === 0) {
                this.scores[currentPlayer] += marble;
                this.currentIndex = (this.currentIndex - 7 + this.circle.length) % this.circle.length;
                this.scores[currentPlayer] += this.circle.splice(this.currentIndex, 1)[0];
            } else {
                this.currentIndex = (this.currentIndex + 2) % this.circle.length;
                this.circle.splice(this.currentIndex, 0, marble);
            }
        }
    }

    getWinningScore(): number {
        return Math.max(...this.scores);
    }
}

const game = new MarbleGame();
game.play();
console.log(game.getWinningScore());