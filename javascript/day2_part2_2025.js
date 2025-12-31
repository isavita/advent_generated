
const fs = require('fs')
const input = fs.readFileSync('input.txt', 'utf8').replace(/\r?\n/g, '').trim()
if (!input) process.exit(0)

const parts = input.split(',').filter(Boolean)
const ranges = parts.map(p => {
  const [a, b] = p.split('-')
  return { s: BigInt(a), e: BigInt(b) }
})

const pow10 = n => 10n ** BigInt(n)

const found = new Set()

for (const { s, e } of ranges) {
  const sLen = s.toString().length
  const eLen = e.toString().length
  for (let totalLen = sLen; totalLen <= eLen; ++totalLen) {
    for (let k = 1; k <= totalLen / 2; ++k) {
      if (totalLen % k) continue
      const reps = totalLen / k
      let M = 0n
      for (let i = 0; i < reps; ++i) M += pow10(i * k)
      const minSeed = pow10(k - 1)
      const maxSeed = pow10(k) - 1n
      const targetMin = (s + M - 1n) / M
      const targetMax = e / M
      let start = targetMin > minSeed ? targetMin : minSeed
      let end = targetMax < maxSeed ? targetMax : maxSeed
      if (start > end) continue
      for (let cur = start; cur <= end; ++cur) found.add(cur * M)
    }
  }
}

let sum = 0n
for (const v of found) sum += v
console.log(`Sum of invalid IDs: ${sum}`)
