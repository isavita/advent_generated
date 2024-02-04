const fs = require('fs');

function validatePassword(policy, password) {
    const [positions, char] = policy.split(' ');
    const [pos1, pos2] = positions.split('-').map(Number);
    return (password[pos1 - 1] === char) !== (password[pos2 - 1] === char);
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const lines = data.trim().split('\n');
    let validCount = 0;

    lines.forEach((line) => {
        const [policy, password] = line.split(': ');
        if (validatePassword(policy, password)) {
            validCount++;
        }
    });

    console.log(validCount);
});