
import * as fs from 'fs';

const Size = 119315717514047n;
const Iter = 101741582076661n;

function modPow(base: bigint, exp: bigint, mod: bigint): bigint {
  let res = 1n;
  base %= mod;
  while (exp > 0n) {
    if (exp % 2n === 1n) res = (res * base) % mod;
    base = (base * base) % mod;
    exp /= 2n;
  }
  return res;
}

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
let offset = 0n;
let increment = 1n;

for (const line of input) {
  if (line === 'deal into new stack') {
    increment = -increment;
    offset += increment;
  } else if (line.startsWith('cut')) {
    const n = BigInt(parseInt(line.split(' ')[1]));
    offset += n * increment;
  } else if (line.startsWith('deal with increment')) {
    const n = BigInt(parseInt(line.split(' ').pop()!));
    increment *= modPow(n, Size - 2n, Size);
  }
}

const finalIncr = modPow(increment, Iter, Size);
const finalOffs =
  ((offset * (1n - modPow(increment, Iter, Size))) % Size) *
  modPow(1n - increment, Size - 2n, Size);

let answer = (2020n * finalIncr + finalOffs) % Size;
if (answer < 0n) answer += Size;

console.log(answer);
