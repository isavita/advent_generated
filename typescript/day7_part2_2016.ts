import * as fs from 'fs';

function supportsTLS(ip: string): boolean {
    let insideBrackets = false;
    let hasABBA = false;

    for (let i = 0; i < ip.length; i++) {
        if (ip[i] === '[') {
            insideBrackets = true;
        } else if (ip[i] === ']') {
            insideBrackets = false;
        } else if (i + 3 < ip.length && ip[i] === ip[i + 3] && ip[i + 1] === ip[i + 2] && ip[i] !== ip[i + 1]) {
            if (insideBrackets) {
                return false;
            } else {
                hasABBA = true;
            }
        }
    }

    return hasABBA;
}

function supportsSSL(ip: string): boolean {
    const supernetSequences: string[] = [];
    const hypernetSequences: string[] = [];
    let currentSequence = '';
    let insideBrackets = false;

    for (let i = 0; i < ip.length; i++) {
        if (ip[i] === '[') {
            if (currentSequence) {
                supernetSequences.push(currentSequence);
                currentSequence = '';
            }
            insideBrackets = true;
        } else if (ip[i] === ']') {
            if (currentSequence) {
                hypernetSequences.push(currentSequence);
                currentSequence = '';
            }
            insideBrackets = false;
        } else {
            currentSequence += ip[i];
        }
    }

    if (currentSequence) {
        supernetSequences.push(currentSequence);
    }

    for (const supernet of supernetSequences) {
        for (let i = 0; i < supernet.length - 2; i++) {
            if (supernet[i] === supernet[i + 2] && supernet[i] !== supernet[i + 1]) {
                const aba = supernet.substring(i, i + 3);
                const bab = aba[1] + aba[0] + aba[1];
                if (hypernetSequences.some(hypernet => hypernet.includes(bab))) {
                    return true;
                }
            }
        }
    }

    return false;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    let tlsCount = 0;
    let sslCount = 0;

    for (const ip of input) {
        if (supportsTLS(ip)) {
            tlsCount++;
        }
        if (supportsSSL(ip)) {
            sslCount++;
        }
    }

    console.log(`Number of IPs that support TLS: ${tlsCount}`);
    console.log(`Number of IPs that support SSL: ${sslCount}`);
}

main();