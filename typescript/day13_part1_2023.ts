const fs = require('fs');

class Mirror {
    constructor() {
        this.rows = [];
        this.cols = [];
    }
}

function parseInput(input) {
    const mirrors = [];

    let mirrorStr = [];
    input.forEach(line => {
        if (line === "") {
            mirrors.push(parseMirror(mirrorStr));
            mirrorStr = [];
        } else {
            mirrorStr.push(line);
        }
    });
    mirrors.push(parseMirror(mirrorStr));

    return mirrors;
}

function parseMirror(mirrorStr) {
    const mirror = new Mirror();
    mirror.rows = new Array(mirrorStr.length).fill(0);
    mirror.cols = new Array(mirrorStr[0].length).fill(0);

    mirrorStr.forEach((line, y) => {
        line.split('').forEach((char, x) => {
            mirror.rows[y] <<= 1;
            mirror.cols[x] <<= 1;
            if (char === '#') {
                mirror.rows[y]++;
                mirror.cols[x]++;
            }
        });
    });

    return mirror;
}

function getMirrorAxis(lines) {
    for (let i = 1; i < lines.length; i++) {
        let isMirror = true;

        for (let j = 0; isMirror && j < Math.min(i, lines.length - i); j++) {
            if (lines[i - 1 - j] !== lines[i + j]) {
                isMirror = false;
            }
        }

        if (isMirror) {
            return i;
        }
    }

    return 0;
}

function getMirrorAxisWithOneSmudge(lines) {
    for (let i = 1; i < lines.length; i++) {
        let isMirror = true;
        let numSmudges = 0;

        for (let j = 0; isMirror && j < Math.min(i, lines.length - i); j++) {
            if (lines[i - 1 - j] !== lines[i + j]) {
                if (numSmudges > 0) {
                    isMirror = false;
                } else {
                    const dif = lines[i - 1 - j] ^ lines[i + j];
                    const isOnlyOneSmudge = (dif & (dif - 1)) === 0;
                    if (isOnlyOneSmudge) {
                        numSmudges++;
                    } else {
                        isMirror = false;
                    }
                }
            }
        }

        if (isMirror && numSmudges === 1) {
            return i;
        }
    }

    return 0;
}

function solve(input) {
    const mirrors = parseInput(input);

    let res = 0;
    mirrors.forEach(mirror => {
        res += getMirrorAxis(mirror.cols);
        res += getMirrorAxis(mirror.rows) * 100;
    });
    return res;
}

function readFile(fileName) {
    const file = fs.readFileSync(fileName, 'utf8');
    return file.trim().split('\n');
}

const input = readFile("input.txt");
console.log(solve(input));