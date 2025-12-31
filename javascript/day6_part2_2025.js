
const fs = require('fs')
function main() {
  const data = fs.readFileSync('input.txt', 'utf8')
  const lines = data.split('\n')
  if (lines.length === 0 || (lines.length === 1 && lines[0] === '')) {
    console.log('Grand total: 0')
    return
  }
  const maxWidth = Math.max(...lines.map(l => l.length))
  const isSep = Array(maxWidth).fill(true)
  for (let x = 0; x < maxWidth; x++) {
    for (let i = 0; i < lines.length; i++) {
      if (x < lines[i].length && !/\s/.test(lines[i][x])) {
        isSep[x] = false
        break
      }
    }
  }
  let grandTotal = 0n
  let inBlock = false, startCol = 0
  for (let x = 0; x < maxWidth; x++) {
    if (!isSep[x]) {
      if (!inBlock) { inBlock = true; startCol = x }
    } else if (inBlock) {
      grandTotal += processBlock(lines, startCol, x - 1)
      inBlock = false
    }
  }
  if (inBlock) grandTotal += processBlock(lines, startCol, maxWidth - 1)
  console.log(`Grand total: ${grandTotal}`)
}
function processBlock(lines, start, end) {
  const nums = []
  let operator = '*'
  for (let c = start; c <= end; c++) {
    let sb = ''
    let hasDigits = false
    for (let r = 0; r < lines.length; r++) {
      if (c < lines[r].length) {
        const ch = lines[r][c]
        if (/\d/.test(ch)) { sb += ch; hasDigits = true }
        else if (ch === '+') operator = '+'
        else if (ch === '*') operator = '*'
      }
    }
    if (hasDigits) nums.push(BigInt(sb))
  }
  if (nums.length === 0) return 0n
  if (operator === '*') {
    return nums.reduce((a, b) => a * b, 1n)
  } else {
    return nums.reduce((a, b) => a + b, 0n)
  }
}
main()
