const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();
const positions = parseInput(input);
const result = solve(positions);
console.log(result);

function solve(input) {
    const [w1, w2] = play([positions[0], positions[1]], [0, 0], 3, true, new Map());
    
    if (w1 > w2) {
        return w1;
    }
    return w2;
}

function play(positions, scores, rollsLeftInTurn, isPlayer1sTurn, memo) {
    const key = `${positions}${scores}${rollsLeftInTurn}${isPlayer1sTurn}`;
    if (memo.has(key)) {
        return memo.get(key);
    }

    let wins1 = 0;
    let wins2 = 0;
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
        let positionsCopy = [...positions];
        positionsCopy[playerIndex] += roll;
        if (positionsCopy[playerIndex] > 10) {
            positionsCopy[playerIndex] -= 10;
        }
        const [r1, r2] = play(positionsCopy, scoresCopy, rollsLeftInTurn - 1, isPlayer1sTurn, memo);
        wins1 += r1;
        wins2 += r2;
    }

    memo.set(key, [wins1, wins2]);
    return [wins1, wins2];
}

function parseInput(input) {
    const ans = [];
    input.split('\n').forEach(line => {
        const [, player, startingPosition] = line.match(/Player (\d) starting position: (\d)/);
        ans.push(parseInt(startingPosition));
    });
    return ans;
}