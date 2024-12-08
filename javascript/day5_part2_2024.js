
const fs = require('node:fs');

function solve() {
    const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
    const rules = {};
    let updates = [];

    let i = 0;
    while (input[i].includes('|')) {
        const [a, b] = input[i].split('|').map(Number);
        rules[a] = rules[a] || [];
        rules[a].push(b);
        i++;
    }

    while (i < input.length) {
        updates.push(input[i].split(',').map(Number));
        i++;
    }

    function isCorrectOrder(update) {
        for (let j = 0; j < update.length; j++) {
            for (let k = j + 1; k < update.length; k++) {
                if (rules[update[j]] && rules[update[j]].includes(update[k])) continue;
                if (rules[update[k]] && rules[update[k]].includes(update[j])) return false;
            }
        }
        return true;
    }

    function getMiddle(arr) {
        return arr[Math.floor(arr.length / 2)];
    }

    let correctlyOrderedSum = 0;
    let incorrectlyOrderedSum = 0;

    for (const update of updates) {
        if (isCorrectOrder(update)) {
            correctlyOrderedSum += getMiddle(update);
        } else {
            //Sort incorrectly ordered updates using topological sort
            const graph = {};
            const inDegree = {};
            const nodes = new Set();

            for (const num of update) {
                graph[num] = [];
                inDegree[num] = 0;
                nodes.add(num);
            }

            for (const num of update) {
                for (const rule in rules) {
                    if (rules[rule].includes(num) && update.includes(parseInt(rule))) {
                        graph[parseInt(rule)].push(num);
                        inDegree[num]++;
                    }
                }
            }

            const queue = [];
            for (const node of nodes) {
                if (inDegree[node] === 0) {
                    queue.push(node);
                }
            }

            const sortedUpdate = [];
            while (queue.length > 0) {
                const node = queue.shift();
                sortedUpdate.push(node);
                for (const neighbor of graph[node]) {
                    inDegree[neighbor]--;
                    if (inDegree[neighbor] === 0) {
                        queue.push(neighbor);
                    }
                }
            }
            incorrectlyOrderedSum += getMiddle(sortedUpdate);
        }
    }

    console.log("Part 1:", correctlyOrderedSum);
    console.log("Part 2:", incorrectlyOrderedSum);
}


solve();
