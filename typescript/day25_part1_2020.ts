import * as fs from 'fs';

const MODULUS = 20201227;
const SUBJECT_NUMBER = 7;

function transform(subject: number, loopSize: number): number {
    let value = 1;
    for (let i = 0; i < loopSize; i++) {
        value = (value * subject) % MODULUS;
    }
    return value;
}

function findLoopSize(publicKey: number): number {
    let value = 1;
    let loopSize = 0;
    while (value !== publicKey) {
        value = (value * SUBJECT_NUMBER) % MODULUS;
        loopSize++;
    }
    return loopSize;
}

function calculateEncryptionKey(publicKey: number, loopSize: number): number {
    return transform(publicKey, loopSize);
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n').map(Number);
    const [cardPublicKey, doorPublicKey] = input;

    const cardLoopSize = findLoopSize(cardPublicKey);
    const encryptionKey = calculateEncryptionKey(doorPublicKey, cardLoopSize);

    console.log(encryptionKey);
}

main();