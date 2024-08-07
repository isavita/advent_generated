import * as fs from 'fs';
import * as readline from 'readline';

const VOWELS = new Set(['a', 'e', 'i', 'o', 'u']);
const DISALLOWED_SUBSTRINGS = ['ab', 'cd', 'pq', 'xy'];

function isNiceString(str: string): boolean {
  let vowelCount = 0;
  let hasDoubleLetter = false;

  for (let i = 0; i < str.length; i++) {
    if (VOWELS.has(str[i])) {
      vowelCount++;
    }
    if (i > 0 && str[i] === str[i - 1]) {
      hasDoubleLetter = true;
    }
  }

  if (vowelCount < 3 || !hasDoubleLetter) {
    return false;
  }

  for (const substring of DISALLOWED_SUBSTRINGS) {
    if (str.includes(substring)) {
      return false;
    }
  }

  return true;
}

async function processInputFile(filePath: string): Promise<void> {
  const fileStream = fs.createReadStream(filePath);
  const rl = readline.createInterface({
    input: fileStream,
    crlfDelay: Infinity,
  });

  let niceStringCount = 0;

  for await (const line of rl) {
    if (isNiceString(line)) {
      niceStringCount++;
    }
  }

  console.log(`Number of nice strings: ${niceStringCount}`);
}

processInputFile('input.txt').catch(console.error);