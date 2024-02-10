
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

let sslCount = 0;
input.forEach(ip => {
  if (supportsSSL(ip)) {
    sslCount++;
  }
});

console.log(sslCount);

function supportsSSL(ip) {
  const insideBrackets = /\[[a-z]+\]/g;
  const bracketContents = ip.match(insideBrackets) || [];

  ip = ip.replace(insideBrackets, '-');
  const abas = findABAs(ip);
  for (const aba of abas) {
    const bab = aba[1] + aba[0] + aba[1];
    for (const bracketContent of bracketContents) {
      if (bracketContent.includes(bab)) {
        return true;
      }
    }
  }

  return false;
}

function findABAs(s) {
  const abas = [];
  for (let i = 0; i < s.length - 2; i++) {
    if (s[i] !== s[i + 1] && s[i] === s[i + 2]) {
      abas.push(s.substring(i, i + 3));
    }
  }
  return abas;
}
