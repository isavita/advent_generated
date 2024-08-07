import * as fs from 'fs';

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const result = solve(input);
    console.log(result);
}

function solve(input: string): number {
    const positions = parseInput(input);
    const memo: { [key: string]: [number, number] } = {};
    const [w1, w2] = play([positions[0], positions[1]], [0, 0], 3, true, memo);
    return w1 > w2 ? w1 : w2;
}

function play(positions: [number, number], scores: [number, number], rollsLeftInTurn: number, isPlayer1sTurn: boolean, memo: { [key: string]: [number, number] }): [number, number] {
    const key = `${positions},${scores},${rollsLeftInTurn},${isPlayer1sTurn}`;
    if (memo[key]) {
        return memo[key];
    }

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

    let wins1 = 0, wins2 = 0;
    for (let roll = 1; roll <= 3; roll++) {
        let positionsCopy = [...positions];
        positionsCopy[playerIndex] = (positionsCopy[playerIndex] + roll) % 10 || 10;
        const [r1, r2] = play([positionsCopy[0], positionsCopy[1]], [scoresCopy[0], scoresCopy[1]], rollsLeftInTurn - 1, isPlayer1sTurn, memo);
        wins1 += r1;
        wins2 += r2;
    }

    memo[key] = [wins1, wins2];
    return [wins1, wins2];
}

function parseInput(input: string): [number, number] {
    const lines = input.split('\n');
    const positions: [number, number] = [0, 0];
    lines.forEach((line, index) => {
        const match = line.match(/Player \d+ starting position: (\d+)/);
        if (match) {
            positions[index] = parseInt(match[1], 10);
        }
    });
    return positions;
}

main();