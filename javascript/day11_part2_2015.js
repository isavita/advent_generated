const fs = require('fs');

function readInput(filename) {
    return fs.readFileSync(filename, 'utf8').trim();
}

function findNextPassword(password) {
    while (true) {
        password = incrementPassword(password);
        if (isValidPassword(password)) {
            break;
        }
    }
    return password;
}

function incrementPassword(password) {
    let chars = password.split('');
    for (let i = chars.length - 1; i >= 0; i--) {
        chars[i] = String.fromCharCode(chars[i].charCodeAt(0) + 1);
        if (chars[i].charCodeAt(0) > 'z'.charCodeAt(0)) {
            chars[i] = 'a';
        } else {
            break;
        }
    }
    return chars.join('');
}

function isValidPassword(password) {
    return hasStraight(password) && !containsInvalidLetters(password) && hasTwoPairs(password);
}

function hasStraight(password) {
    for (let i = 0; i < password.length - 2; i++) {
        if (password.charCodeAt(i) + 1 === password.charCodeAt(i + 1) && password.charCodeAt(i) + 2 === password.charCodeAt(i + 2)) {
            return true;
        }
    }
    return false;
}

function containsInvalidLetters(password) {
    return password.includes('i') || password.includes('o') || password.includes('l');
}

function hasTwoPairs(password) {
    let count = 0;
    for (let i = 0; i < password.length - 1; i++) {
        if (password[i] === password[i + 1]) {
            count++;
            i++; // Skip the next character
        }
    }
    return count >= 2;
}

const currentPassword = readInput('input.txt');
const firstNewPassword = findNextPassword(currentPassword);
const secondNewPassword = findNextPassword(firstNewPassword);
console.log(secondNewPassword);