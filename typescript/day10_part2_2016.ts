import * as fs from 'fs';

interface Bot {
    low: number | null;
    high: number | null;
    lowTarget: string | null;
    highTarget: string | null;
    lowTargetType: 'bot' | 'output' | null;
    highTargetType: 'bot' | 'output' | null;
}

const bots: { [key: number]: Bot } = {};
const outputs: { [key: number]: number } = {};
const instructions: string[] = [];

function parseInput(filename: string): void {
    const data = fs.readFileSync(filename, 'utf-8');
    data.split('\n').forEach(line => {
        if (line.startsWith('value')) {
            const [, value, bot] = line.match(/value (\d+) goes to bot (\d+)/)!;
            const botNum = parseInt(bot, 10);
            const valueNum = parseInt(value, 10);
            if (!bots[botNum]) {
                bots[botNum] = { low: null, high: null, lowTarget: null, highTarget: null, lowTargetType: null, highTargetType: null };
            }
            if (bots[botNum].low === null || valueNum < bots[botNum].low!) {
                bots[botNum].high = bots[botNum].low;
                bots[botNum].low = valueNum;
            } else {
                bots[botNum].high = valueNum;
            }
            processBot(botNum);
        } else {
            instructions.push(line);
        }
    });
}

function processBot(botNum: number): void {
    const bot = bots[botNum];
    if (bot.low !== null && bot.high !== null && bot.lowTarget !== null && bot.highTarget !== null) {
        if (bot.lowTargetType === 'bot') {
            giveValueToBot(parseInt(bot.lowTarget, 10), bot.low);
        } else if (bot.lowTargetType === 'output') {
            outputs[parseInt(bot.lowTarget, 10)] = bot.low;
        }
        if (bot.highTargetType === 'bot') {
            giveValueToBot(parseInt(bot.highTarget, 10), bot.high);
        } else if (bot.highTargetType === 'output') {
            outputs[parseInt(bot.highTarget, 10)] = bot.high;
        }
        bot.low = null;
        bot.high = null;
    }
}

function giveValueToBot(botNum: number, value: number): void {
    if (!bots[botNum]) {
        bots[botNum] = { low: null, high: null, lowTarget: null, highTarget: null, lowTargetType: null, highTargetType: null };
    }
    if (bots[botNum].low === null || value < bots[botNum].low!) {
        bots[botNum].high = bots[botNum].low;
        bots[botNum].low = value;
    } else {
        bots[botNum].high = value;
    }
    processBot(botNum);
}

function parseInstructions(): void {
    instructions.forEach(line => {
        const [, bot, lowTargetType, lowTarget, highTargetType, highTarget] = line.match(/bot (\d+) gives low to (\w+) (\d+) and high to (\w+) (\d+)/)!;
        const botNum = parseInt(bot, 10);
        if (!bots[botNum]) {
            bots[botNum] = { low: null, high: null, lowTarget: null, highTarget: null, lowTargetType: null, highTargetType: null };
        }
        bots[botNum].lowTarget = lowTarget;
        bots[botNum].highTarget = highTarget;
        bots[botNum].lowTargetType = lowTargetType as 'bot' | 'output';
        bots[botNum].highTargetType = highTargetType as 'bot' | 'output';
        processBot(botNum);
    });
}

function findBotComparingValues(value1: number, value2: number): number | null {
    for (const botNum in bots) {
        const bot = bots[botNum];
        if (bot.low === value1 && bot.high === value2) {
            return parseInt(botNum, 10);
        }
        if (bot.low === value2 && bot.high === value1) {
            return parseInt(botNum, 10);
        }
    }
    return null;
}

function main(): void {
    parseInput('input.txt');
    parseInstructions();

    const botNumber = findBotComparingValues(61, 17);
    if (botNumber !== null) {
        console.log(`The bot responsible for comparing value-61 microchips with value-17 microchips is bot ${botNumber}.`);
    } else {
        console.log('No bot found comparing value-61 microchips with value-17 microchips.');
    }

    const outputProduct = outputs[0] * outputs[1] * outputs[2];
    console.log(`The product of the values in outputs 0, 1, and 2 is ${outputProduct}.`);
}

main();