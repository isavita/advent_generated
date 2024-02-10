const fs = require('fs');

function isValidPassword(password) {
    const s = password.toString();
    let hasDouble = false;

    for (let i = 0; i < s.length - 1; i++) {
        if (s[i] > s[i + 1]) {
            return false;
        }
        if (s[i] === s[i + 1]) {
            if ((i === 0 || s[i] !== s[i - 1]) && (i + 2 >= s.length || s[i] !== s[i + 2])) {
                hasDouble = true;
            }
        }
    }

    return hasDouble;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const rangeStr = data.trim();
    const ranges = rangeStr.split('-');
    const start = parseInt(ranges[0]);
    const end = parseInt(ranges[1]);

    let count = 0;
    for (let i = start; i <= end; i++) {
        if (isValidPassword(i)) {
            count++;
        }
    }

    console.log(count);
});