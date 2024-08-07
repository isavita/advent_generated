import * as fs from 'fs';

const shapeScore: Record<string, number> = { 'X': 1, 'Y': 2, 'Z': 3 }; // Rock, Paper, Scissors
const outcomeScore: Record<string, number> = { 'win': 6, 'draw': 3, 'lose': 0 };

const getRoundScore = (opponent: string, player: string): number => {
    const outcomes: Record<string, Record<string, string>> = {
        'A': { 'X': 'draw', 'Y': 'win', 'Z': 'lose' }, // Rock
        'B': { 'X': 'lose', 'Y': 'draw', 'Z': 'win' }, // Paper
        'C': { 'X': 'win', 'Y': 'lose', 'Z': 'draw' }  // Scissors
    };

    const outcome = outcomes[opponent][player];
    return shapeScore[player] + outcomeScore[outcome];
};

const calculateTotalScore = (input: string): number => {
    return input.trim().split('\n').reduce((total, line) => {
        const [opponent, player] = line.split(' ');
        return total + getRoundScore(opponent, player);
    }, 0);
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const totalScore = calculateTotalScore(input);
    console.log(totalScore);
};

main();