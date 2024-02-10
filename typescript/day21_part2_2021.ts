const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();
const positions = input.split('\n').map(line => {
    const [, , startingPosition] = line.match(/Player (\d) starting position: (\d)/);
    return parseInt(startingPosition);
});

const memo = new Map();

function play(positions, scores, rollsLeftInTurn, isPlayer1sTurn) {
    const key = `${positions}${scores}${rollsLeftInTurn}${isPlayer1sTurn}`;
    if (memo.has(key)) {
        return memo.get(key);
    }

    let [wins1, wins2] = [0, 0];
    let playerIndex = isPlayer1sTurn ? 0 : 1;
    let scoresCopy = [...scores];

    if (rollsLeftInTurn === 0) {
        scoresCopy[playerIndex] += positions[playerIndex];
        if (scoresCopy[playerIndex] >= 21) {
            return playerIndex === 0 ? [1, 0] : [0, 1];
        }
        isPlayer1sTurn = !isPlayer1sTurn;
        rollsLeftInTurn = 3;
        playerIndex = (playerIndex + 1) % 2;
    }

    for (let roll = 1; roll <= 3; roll++) {
        const positionsCopy = [...positions];
        positionsCopy[playerIndex] += roll;
        if (positionsCopy[playerIndex] > 10) {
            positionsCopy[playerIndex] -= 10;
        }
        const [r1, r2] = play(positionsCopy, scoresCopy, rollsLeftInTurn - 1, isPlayer1sTurn);
        wins1 += r1;
        wins2 += r2;
    }

    memo.set(key, [wins1, wins2]);
    return [wins1, wins2];
}

function solve(input) {
    const [w1, w2] = play([positions[0], positions[1]], [0, 0], 3, true);
    return w1 > w2 ? w1 : w2;
}

const result = solve(input);
console.log(result);