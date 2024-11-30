const fs = require('fs');

// Read input from file
const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

// Constants for the problem
const DECK_SIZE = 119315717514047n;
const ITERATIONS = 101741582076661n;
const TARGET_POSITION = 2020n;

// Extended Euclidean Algorithm for modular multiplicative inverse
function modInverse(a, m) {
    let t = 0n, newT = 1n;
    let r = m, newR = a;
    
    while (newR != 0n) {
        let quotient = r / newR;
        [t, newT] = [newT, t - quotient * newT];
        [r, newR] = [newR, r - quotient * newR];
    }
    
    if (t < 0n) t += m;
    return t;
}

// Process a single shuffle instruction to get linear transformation parameters
function processInstruction(line, deckSize) {
    if (line === 'deal into new stack') {
        return [-1n, -1n, deckSize];
    }
    
    if (line.startsWith('cut')) {
        const n = BigInt(line.split(' ')[1]);
        return [1n, -n, deckSize];
    }
    
    if (line.startsWith('deal with increment')) {
        const n = BigInt(line.split(' ')[3]);
        return [n, 0n, deckSize];
    }
}

// Combine two linear transformations
function combine(a1, b1, a2, b2, m) {
    const a = (a1 * a2) % m;
    const b = (a2 * b1 + b2) % m;
    return [a, b];
}

// Main solution
function solve() {
    let [a, b] = [1n, 0n];
    
    // Process all instructions to get single linear transformation
    for (const line of input) {
        const [na, nb, m] = processInstruction(line, DECK_SIZE);
        [a, b] = combine(a, b, na, nb, DECK_SIZE);
    }
    
    // Calculate parameters for repeated iterations using geometric series
    let power = ITERATIONS;
    let resultA = 1n;
    let resultB = 0n;
    let currentA = a;
    let currentB = b;
    
    while (power > 0n) {
        if (power % 2n === 1n) {
            [resultA, resultB] = combine(resultA, resultB, currentA, currentB, DECK_SIZE);
        }
        [currentA, currentB] = combine(currentA, currentB, currentA, currentB, DECK_SIZE);
        power /= 2n;
    }
    
    // Calculate final position using modular arithmetic
    const numerator = (TARGET_POSITION - resultB);
    const denominator = resultA;
    const result = (numerator * modInverse(denominator, DECK_SIZE)) % DECK_SIZE;
    if (result < 0n) result += DECK_SIZE;
    
    console.log(result.toString());
}

solve();
