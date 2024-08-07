import * as fs from 'fs';
import * as readline from 'readline';

interface Instruction {
    action: string;
    from: { x: number; y: number };
    to: { x: number; y: number };
}

function parseInstruction(line: string): Instruction {
    const action = line.match(/(turn on|turn off|toggle)/)![0];
    const [_, fromX, fromY, toX, toY] = line.match(/(\d+),(\d+) through (\d+),(\d+)/)!.map(Number);
    return { action, from: { x: fromX, y: fromY }, to: { x: toX, y: toY } };
}

function processInstructions(instructions: Instruction[]): [number, number] {
    const gridSize = 1000;
    const grid = Array.from({ length: gridSize }, () => Array(gridSize).fill(0));

    for (const { action, from, to } of instructions) {
        for (let y = from.y; y <= to.y; y++) {
            for (let x = from.x; x <= to.x; x++) {
                if (action === 'turn on') {
                    grid[y][x] = 1;
                } else if (action === 'turn off') {
                    grid[y][x] = 0;
                } else if (action === 'toggle') {
                    grid[y][x] = 1 - grid[y][x];
                }
            }
        }
    }

    const litLights = grid.flat().reduce((sum, light) => sum + light, 0);

    const brightnessGrid = Array.from({ length: gridSize }, () => Array(gridSize).fill(0));

    for (const { action, from, to } of instructions) {
        for (let y = from.y; y <= to.y; y++) {
            for (let x = from.x; x <= to.x; x++) {
                if (action === 'turn on') {
                    brightnessGrid[y][x] += 1;
                } else if (action === 'turn off') {
                    brightnessGrid[y][x] = Math.max(0, brightnessGrid[y][x] - 1);
                } else if (action === 'toggle') {
                    brightnessGrid[y][x] += 2;
                }
            }
        }
    }

    const totalBrightness = brightnessGrid.flat().reduce((sum, brightness) => sum + brightness, 0);

    return [litLights, totalBrightness];
}

async function main() {
    const fileStream = fs.createReadStream('input.txt');
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const instructions: Instruction[] = [];

    for await (const line of rl) {
        instructions.push(parseInstruction(line));
    }

    const [litLights, totalBrightness] = processInstructions(instructions);

    console.log(`Number of lights lit: ${litLights}`);
    console.log(`Total brightness of all lights: ${totalBrightness}`);
}

main().catch(console.error);