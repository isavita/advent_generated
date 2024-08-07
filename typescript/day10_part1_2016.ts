import * as fs from 'fs';
import * as readline from 'readline';

interface BotInstruction {
    lowType: 'bot' | 'output';
    low: number;
    highType: 'bot' | 'output';
    high: number;
}

interface ValueInstruction {
    value: number;
    bot: number;
}

interface Bot {
    values: number[];
    instruction?: BotInstruction;
}

const bots: { [key: number]: Bot } = {};
const outputs: { [key: number]: number[] } = {};
const valueInstructions: ValueInstruction[] = [];
const botInstructions: { [key: number]: BotInstruction } = {};

async function processLineByLine() {
    const fileStream = fs.createReadStream('input.txt');

    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    for await (const line of rl) {
        if (line.startsWith('value')) {
            const match = line.match(/value (\d+) goes to bot (\d+)/);
            if (match) {
                valueInstructions.push({ value: parseInt(match[1]), bot: parseInt(match[2]) });
            }
        } else if (line.startsWith('bot')) {
            const match = line.match(/bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)/);
            if (match) {
                const bot = parseInt(match[1]);
                botInstructions[bot] = {
                    lowType: match[2] as 'bot' | 'output',
                    low: parseInt(match[3]),
                    highType: match[4] as 'bot' | 'output',
                    high: parseInt(match[5]),
                };
            }
        }
    }
}

function initializeBots() {
    for (const bot in botInstructions) {
        bots[bot] = { values: [], instruction: botInstructions[bot] };
    }
}

function giveValueToBot(value: number, bot: number) {
    if (!bots[bot]) {
        bots[bot] = { values: [] };
    }
    bots[bot].values.push(value);
    bots[bot].values.sort((a, b) => a - b);

    if (bots[bot].values.length === 2) {
        const low = bots[bot].values[0];
        const high = bots[bot].values[1];
        if ((low === 17 && high === 61) || (low === 61 && high === 17)) {
            console.log(`Bot ${bot} is responsible for comparing value-61 microchips with value-17 microchips`);
        }

        const instruction = bots[bot].instruction;
        if (instruction) {
            if (instruction.lowType === 'bot') {
                giveValueToBot(low, instruction.low);
            } else {
                if (!outputs[instruction.low]) {
                    outputs[instruction.low] = [];
                }
                outputs[instruction.low].push(low);
            }

            if (instruction.highType === 'bot') {
                giveValueToBot(high, instruction.high);
            } else {
                if (!outputs[instruction.high]) {
                    outputs[instruction.high] = [];
                }
                outputs[instruction.high].push(high);
            }
        }

        bots[bot].values = [];
    }
}

async function main() {
    await processLineByLine();
    initializeBots();
    valueInstructions.forEach(instruction => giveValueToBot(instruction.value, instruction.bot));
}

main().catch(console.error);