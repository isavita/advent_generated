import * as fs from 'fs';

function isValidPassphrase(passphrase: string): boolean {
    const words = passphrase.split(' ');
    const wordSet = new Set(words);
    return words.length === wordSet.size;
}

function isValidPassphrasePartTwo(passphrase: string): boolean {
    const words = passphrase.split(' ');
    for (let i = 0; i < words.length; i++) {
        for (let j = i + 1; j < words.length; j++) {
            if (areAnagrams(words[i], words[j])) {
                return false;
            }
        }
    }
    return true;
}

function areAnagrams(word1: string, word2: string): boolean {
    if (word1.length !== word2.length) return false;
    const sortedWord1 = word1.split('').sort().join('');
    const sortedWord2 = word2.split('').sort().join('');
    return sortedWord1 === sortedWord2;
}

function countValidPassphrases(input: string[], isValid: (passphrase: string) => boolean): number {
    return input.filter(isValid).length;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

    const validPassphrasesPartOne = countValidPassphrases(input, isValidPassphrase);
    console.log(`Part One: ${validPassphrasesPartOne}`);

    const validPassphrasesPartTwo = countValidPassphrases(input, isValidPassphrasePartTwo);
    console.log(`Part Two: ${validPassphrasesPartTwo}`);
}

main();