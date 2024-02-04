const fs = require('fs');

function isPrime(n) {
    for (let i = 2; i * i <= n; i++) {
        if (n % i === 0) {
            return false;
        }
    }
    return true;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const input = data.split('\n').map(Number);
    const b = 57 * 100 + 100000;
    const c = b + 17000;
    let h = 0;

    for (let x = b; x <= c; x += 17) {
        if (!isPrime(x)) {
            h++;
        }
    }

    console.log(h);
});