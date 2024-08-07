import * as fs from 'fs';

const shapeScores: Record<string, number> = { 'X': 1, 'Y': 2, 'Z': 3 };
const outcomeScores: Record<string, number> = { 'win': 6, 'draw': 3, 'lose': 0 };
const opponentShapes: Record<string, string> = { 'A': 'X', 'B': 'Y', 'C': 'Z' };

const calculateScore = (opponent: keyof typeof opponentShapes, outcome: string): number => {
    const playerShape = determineShape(opponent, outcome);
    const shapeScore = shapeScores[playerShape];
    const outcomeScore = getOutcomeScore(opponent, playerShape);
    return shapeScore + outcomeScore;
};

const determineShape = (opponent: keyof typeof opponentShapes, outcome: string): string => {
    if (outcome === 'Y') return opponentShapes[opponent]; // Draw
    if (outcome === 'X') return loseShape(opponent); // Lose
    return winShape(opponent); // Win
};

const loseShape = (opponent: keyof typeof opponentShapes): string => {
    return opponent === 'A' ? 'Z' : opponent === 'B' ? 'X' : 'Y';
};

const winShape = (opponent: keyof typeof opponentShapes): string => {
    return opponent === 'A' ? 'Y' : opponent === 'B' ? 'Z' : 'X';
};

const getOutcomeScore = (opponent: keyof typeof opponentShapes, player: string): number => {
    if (opponentShapes[opponent] === player) return outcomeScores.draw;
    if ((opponent === 'A' && player === 'Y') || (opponent === 'B' && player === 'Z') || (opponent === 'C' && player === 'X')) {
        return outcomeScores.win;
    }
    return outcomeScores.lose;
};

const main = () => {
    const data = fs.readFileSync('input.txt', 'utf-8');
    const rounds = data.trim().split('\n');
    const totalScore = rounds.reduce((score, round) => {
        const [opponent, outcome] = round.split(' ') as [keyof typeof opponentShapes, string];
        return score + calculateScore(opponent, outcome);
    }, 0);
    console.log(totalScore);
};

main();