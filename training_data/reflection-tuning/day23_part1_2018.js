const fs = require('fs');

function solveProblem(input) {
    const nanobots = input.trim().split('\n').map(line => {
        const [pos, r] = line.split(', r=');
        const [x, y, z] = pos.slice(5, -1).split(',').map(Number);
        return { x, y, z, r: Number(r) };
    });

    let strongest = nanobots[0];
    for (const bot of nanobots) {
        if (bot.r > strongest.r) {
            strongest = bot;
        }
    }

    let inRangeCount = 0;
    for (const bot of nanobots) {
        const distance = Math.abs(bot.x - strongest.x) +
                         Math.abs(bot.y - strongest.y) +
                         Math.abs(bot.z - strongest.z);
        if (distance <= strongest.r) {
            inRangeCount++;
        }
    }

    return inRangeCount;
}

const input = fs.readFileSync('input.txt', 'utf8');
console.log(solveProblem(input));
