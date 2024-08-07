import * as fs from 'fs';

function decodeString(str: string): string {
    let result = '';
    let i = 1; // Skip the first "
    while (i < str.length - 1) { // Skip the last "
        if (str[i] === '\\') {
            i++;
            if (str[i] === '\\' || str[i] === '"') {
                result += str[i];
            } else if (str[i] === 'x') {
                result += String.fromCharCode(parseInt(str.slice(i + 1, i + 3), 16));
                i += 2;
            }
        } else {
            result += str[i];
        }
        i++;
    }
    return result;
}

function encodeString(str: string): string {
    let result = '"';
    for (let i = 0; i < str.length; i++) {
        if (str[i] === '\\' || str[i] === '"') {
            result += '\\';
        }
        result += str[i];
    }
    result += '"';
    return result;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

    let codeLength = 0;
    let memoryLength = 0;
    let encodedLength = 0;

    for (const line of input) {
        codeLength += line.length;
        memoryLength += decodeString(line).length;
        encodedLength += encodeString(line).length;
    }

    const part1Result = codeLength - memoryLength;
    const part2Result = encodedLength - codeLength;

    console.log(`Part 1: ${part1Result}`);
    console.log(`Part 2: ${part2Result}`);
}

main();