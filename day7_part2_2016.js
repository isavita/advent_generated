const fs = require('fs');
const readline = require('readline');
const regex = /\[[a-z]+\]/g;

const readInterface = readline.createInterface({
    input: fs.createReadStream('input.txt'),
    output: process.stdout,
    console: false
});

let sslCount = 0;

readInterface.on('line', function(line) {
    if (supportsSSL(line)) {
        sslCount++;
    }
});

readInterface.on('close', function() {
    console.log(sslCount);
});

function supportsSSL(ip) {
    const bracketContents = ip.match(regex) || [];

    ip = ip.replace(regex, "-");
    const abas = findABAs(ip);

    for (const aba of abas) {
        const bab = aba[1] + aba[0] + aba[1];
        for (const bracketContent of bracketContents) {
            if (bracketContent.includes(bab)) {
                return true;
            }
        }
    }

    return false;
}

function findABAs(s) {
    const abas = [];
    for (let i = 0; i < s.length - 2; i++) {
        if (s[i] !== s[i + 1] && s[i] === s[i + 2]) {
            abas.push(s.substring(i, i + 3));
        }
    }
    return abas;
}