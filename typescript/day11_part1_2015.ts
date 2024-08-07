import * as fs from 'fs';

// Function to read the input password from a file
function readInput(filePath: string): string {
    return fs.readFileSync(filePath, 'utf-8').trim();
}

// Function to increment the password
function incrementPassword(password: string): string {
    let chars = password.split('');
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

// Function to check if the password contains an increasing straight of at least three letters
function hasIncreasingStraight(password: string): boolean {
    for (let i = 0; i < password.length - 2; i++) {
        if (password.charCodeAt(i + 1) === password.charCodeAt(i) + 1 &&
            password.charCodeAt(i + 2) === password.charCodeAt(i) + 2) {
            return true;
        }
    }
    return false;
}

// Function to check if the password contains any of the forbidden letters
function containsForbiddenLetters(password: string): boolean {
    return /[iol]/.test(password);
}

// Function to check if the password contains at least two different, non-overlapping pairs of letters
function hasTwoPairs(password: string): boolean {
    let pairs = 0;
    let i = 0;

    while (i < password.length - 1) {
        if (password[i] === password[i + 1]) {
            pairs++;
            i += 2; // Skip the next character to avoid overlapping pairs
        } else {
            i++;
        }
    }

    return pairs >= 2;
}

// Function to find the next valid password
function findNextPassword(password: string): string {
    do {
        password = incrementPassword(password);
    } while (!hasIncreasingStraight(password) || containsForbiddenLetters(password) || !hasTwoPairs(password));

    return password;
}

// Main function to read input, process, and print the output
function main() {
    const inputPassword = readInput('input.txt');
    const nextPassword = findNextPassword(inputPassword);
    console.log(nextPassword);
}

// Run the main function
main();