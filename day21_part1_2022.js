const fs = require('fs');

const jobs = {};
const results = {};

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const lines = data.split('\n');
    lines.forEach(line => {
        const parts = line.split(': ');
        jobs[parts[0]] = parts[1];
    });

    console.log(calculate("root", jobs, results));
});

function calculate(monkey, jobs, results) {
    if (results[monkey] !== undefined) {
        return results[monkey];
    }

    if (!jobs[monkey]) {
        throw new Error("Monkey not found: " + monkey);
    }

    const job = jobs[monkey];

    if (!isNaN(job)) {
        results[monkey] = parseInt(job);
        return parseInt(job);
    }

    const parts = job.split(' ');
    const a = calculate(parts[0], jobs, results);
    const b = calculate(parts[2], jobs, results);

    let result;
    switch (parts[1]) {
        case "+":
            result = a + b;
            break;
        case "-":
            result = a - b;
            break;
        case "*":
            result = a * b;
            break;
        case "/":
            result = Math.floor(a / b);
            break;
        default:
            throw new Error("Unknown operation: " + parts[1]);
    }

    results[monkey] = result;
    return result;
}