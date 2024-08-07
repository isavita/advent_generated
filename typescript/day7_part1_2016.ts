import * as fs from 'fs';

function supportsTLS(ip: string): boolean {
    let insideBrackets = false;
    let hasABBAOutsideBrackets = false;
    let hasABBAInsideBrackets = false;

    for (let i = 0; i < ip.length; i++) {
        if (ip[i] === '[') {
            insideBrackets = true;
        } else if (ip[i] === ']') {
            insideBrackets = false;
        } else if (i + 3 < ip.length) {
            const segment = ip.slice(i, i + 4);
            if (segment[0] !== segment[1] && segment[0] === segment[3] && segment[1] === segment[2]) {
                if (insideBrackets) {
                    hasABBAInsideBrackets = true;
                } else {
                    hasABBAOutsideBrackets = true;
                }
            }
        }
    }

    return hasABBAOutsideBrackets && !hasABBAInsideBrackets;
}

function countTLSIPs(input: string): number {
    const lines = input.trim().split('\n');
    let count = 0;

    for (const line of lines) {
        if (supportsTLS(line)) {
            count++;
        }
    }

    return count;
}

function main() {
    fs.readFile('input.txt', 'utf8', (err, data) => {
        if (err) {
            console.error('Error reading file:', err);
            return;
        }

        const result = countTLSIPs(data);
        console.log(result);
    });
}

main();