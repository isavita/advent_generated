function manhattanDistance(a, b) {
    return Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]) + Math.abs(a[2] - b[2]);
}

function isInRange(bot, point) {
    return manhattanDistance(bot.pos, point) <= bot.r;
}

function findBestPoint(nanobots) {
    // Find the bounding box for all nanobots
    let min = [Infinity, Infinity, Infinity];
    let max = [-Infinity, -Infinity, -Infinity];
    for (let bot of nanobots) {
        for (let i = 0; i < 3; i++) {
            min[i] = Math.min(min[i], bot.pos[i] - bot.r);
            max[i] = Math.max(max[i], bot.pos[i] + bot.r);
        }
    }

    let queue = [{ min, max, count: nanobots.length }];
    let bestCount = 0;
    let bestDist = Infinity;

    while (queue.length > 0) {
        let { min, max, count } = queue.shift();

        // If this region can't beat the best, skip it
        let maxDist = Math.max(...min.map(Math.abs));
        if (count < bestCount || (count === bestCount && maxDist >= bestDist)) {
            continue;
        }

        // If we're down to a single point, check it
        if (min[0] === max[0] && min[1] === max[1] && min[2] === max[2]) {
            let inRange = nanobots.filter(bot => isInRange(bot, min)).length;
            let dist = manhattanDistance([0, 0, 0], min);
            if (inRange > bestCount || (inRange === bestCount && dist < bestDist)) {
                bestCount = inRange;
                bestDist = dist;
            }
            continue;
        }

        // Divide the region into octants and add them to the queue
        let mid = min.map((v, i) => Math.floor((v + max[i]) / 2));
        for (let dx = 0; dx < 2; dx++) {
            for (let dy = 0; dy < 2; dy++) {
                for (let dz = 0; dz < 2; dz++) {
                    let newMin = [min[0] + dx * (mid[0] - min[0] + 1), min[1] + dy * (mid[1] - min[1] + 1), min[2] + dz * (mid[2] - min[2] + 1)];
                    let newMax = [dx ? max[0] : mid[0], dy ? max[1] : mid[1], dz ? max[2] : mid[2]];
                    let newCount = nanobots.filter(bot => {
                        return Math.max(
                            Math.abs(bot.pos[0] - (newMin[0] + newMax[0]) / 2),
                            Math.abs(bot.pos[1] - (newMin[1] + newMax[1]) / 2),
                            Math.abs(bot.pos[2] - (newMin[2] + newMax[2]) / 2)
                        ) <= (bot.r + (newMax[0] - newMin[0] + newMax[1] - newMin[1] + newMax[2] - newMin[2]) / 2);
                    }).length;
                    queue.push({ min: newMin, max: newMax, count: newCount });
                }
            }
        }
    }

    return bestDist;
}

// Example usage:
const nanobots = [
    { pos: [10, 12, 12], r: 2 },
    { pos: [12, 14, 12], r: 2 },
    { pos: [16, 12, 12], r: 4 },
    { pos: [14, 14, 14], r: 6 },
    { pos: [50, 50, 50], r: 200 },
    { pos: [10, 10, 10], r: 5 }
];

console.log(findBestPoint(nanobots)); // Should output 36
