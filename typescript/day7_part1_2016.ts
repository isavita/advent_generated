const fs = require('fs');

function main() {
    const input = fs.readFileSync('input.txt', 'utf8').split('\n');
    let tlsCount = 0;

    input.forEach(ip => {
        if (supportsTLS(ip)) {
            tlsCount++;
        }
    });

    console.log(tlsCount);
}

function supportsTLS(ip) {
    const insideBrackets = /\[[a-z]+\]/g;
    const bracketContents = ip.match(insideBrackets) || [];

    for (const bracketContent of bracketContents) {
        if (containsABBA(bracketContent)) {
            return false;
        }
    }

    ip = ip.replace(insideBrackets, "-");
    return containsABBA(ip);
}

function containsABBA(s) {
    for (let i = 0; i < s.length - 3; i++) {
        if (s[i] !== s[i + 1] && s[i] === s[i + 3] && s[i + 1] === s[i + 2]) {
            return true;
        }
    }
    return false;
}

main();