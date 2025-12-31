
import { readFileSync } from 'fs'

const data = readFileSync('input.txt', 'utf8')
const rawLines = data.split(/\r?\n/)
const lines = rawLines.filter((_, i) => i < rawLines.length - 1 || rawLines[i] !== '')
const linecnt = lines.length
if (linecnt === 0) {
    console.log('Grand total: 0')
    process.exit(0)
}
let maxw = 0
for (const ln of lines) if (ln.length > maxw) maxw = ln.length
const isSep = new Array(maxw).fill(true)
for (let x = 0; x < maxw; ++x) {
    for (let r = 0; r < linecnt; ++r) {
        const ch = x < lines[r].length ? lines[r][x] : ' '
        if (!/\s/.test(ch)) { isSep[x] = false; break }
    }
}
let grandTotal = 0n
let inBlock = false, start = 0
for (let x = 0; x < maxw; ++x) {
    if (!isSep[x]) {
        if (!inBlock) { inBlock = true; start = x }
    } else {
        if (inBlock) {
            processBlock(start, x - 1)
            inBlock = false
        }
    }
}
if (inBlock) processBlock(start, maxw - 1)

function processBlock(s: number, e: number) {
    const nums: string[] = []
    let op: '+' | '*' = '+'
    for (let c = s; c <= e; ++c) {
        let buf = ''
        for (let r = 0; r < linecnt; ++r) {
            if (c >= lines[r].length) continue
            const ch = lines[r][c]
            if (/[0-9]/.test(ch)) buf += ch
            else if (ch === '+' || ch === '*') op = ch as any
        }
        if (buf.length) nums.push(buf)
    }
    if (nums.length === 0) return
    let blockRes = op === '*' ? 1n : 0n
    for (const s of nums) {
        const v = BigInt(s)
        blockRes = op === '*' ? blockRes * v : blockRes + v
    }
    grandTotal += blockRes
}

console.log(`Grand total: ${grandTotal.toString()}`)
