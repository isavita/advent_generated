import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim();
const cups = input.split('').map(Number);
const moves = 100;

const simulateCrabCups = (cups: number[], moves: number): number[] => {
    const maxCup = Math.max(...cups);
    const cupMap = new Map<number, number>();
    for (let i = 0; i < cups.length; i++) {
        cupMap.set(cups[i], cups[(i + 1) % cups.length]);
    }

    let currentCup = cups[0];

    for (let move = 0; move < moves; move++) {
        const pickedUp: (number | undefined)[] = [
            cupMap.get(currentCup),
            cupMap.get(cupMap.get(currentCup) || 0) || 0,
            cupMap.get(cupMap.get(cupMap.get(currentCup) || 0) || 0) || 0
        ];

        cupMap.set(currentCup, cupMap.get(pickedUp[2] || 0) || 0);

        let destinationCup = currentCup - 1;
        while (pickedUp.includes(destinationCup) || destinationCup <= 0) {
            destinationCup = destinationCup <= 0 ? maxCup : destinationCup - 1;
        }

        cupMap.set(pickedUp[2] || 0, cupMap.get(destinationCup) || 0);
        cupMap.set(destinationCup, pickedUp[0] || 0);

        currentCup = cupMap.get(currentCup) || 0;
    }

    const result: number[] = [];
    let cup = cupMap.get(1) || 0;
    while (cup !== 1) {
        result.push(cup);
        cup = cupMap.get(cup) || 0;
    }

    return result;
};

const result = simulateCrabCups(cups, moves);
console.log(result.join(''));