import * as fs from 'fs';

// Function to increment the password
function incrementPassword(password: string): string {
    const chars = password.split('');
    let i = chars.length - 1;

    while (i >= 0) {
        if (chars[i] === 'z') {
            chars[i] = 'a';
            i--;
        } else {
            chars[i] = String.fromCharCode(chars[i].charCodeAt(0) + 1);
            break;
        }
    }

    return chars.join('');
}

// Function to check if the password is valid
function isValidPassword(password: string): boolean {
    if (/[iol]/.test(password)) return false;
    if (!(/([a-z])\1.*([a-z])\2/.test(password))) return false;
    if (!(/(?:(abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn|mno|nop|opq|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz))/.test(password))) return false;

    return true;
}

// Function to find the next valid password
function findNextValidPassword(currentPassword: string): string {
    let nextPassword = incrementPassword(currentPassword);

    while (!isValidPassword(nextPassword)) {
        nextPassword = incrementPassword(nextPassword);
    }

    return nextPassword;
}

// Read the input from the file
const input = fs.readFileSync('input.txt', 'utf-8').trim();

// Find and print the next valid password
const nextPassword = findNextValidPassword(input);
console.log(nextPassword);

// Find and print the next valid password after the first one
const nextNextPassword = findNextValidPassword(nextPassword);
console.log(nextNextPassword);