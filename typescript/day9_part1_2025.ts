
import { readFileSync } from 'fs'

function main() {
  const data = readFileSync('input.txt', 'utf8')
  const xs: bigint[] = []
  const ys: bigint[] = []

  for (const line of data.split('\n')) {
    const trimmed = line.trim()
    if (!trimmed) continue
    const idx = trimmed.indexOf(',')
    if (idx === -1) continue
    const x = BigInt(trimmed.slice(0, idx).trim())
    const y = BigInt(trimmed.slice(idx + 1).trim())
    xs.push(x)
    ys.push(y)
  }

  const n = xs.length
  if (n === 0) return

  let maxArea = 1n

  for (let i = 0; i < n; i++) {
    const x1 = xs[i], y1 = ys[i]
    for (let j = i + 1; j < n; j++) {
      const width = (x1 > xs[j] ? x1 - xs[j] : xs[j] - x1) + 1n
      const height = (y1 > ys[j] ? y1 - ys[j] : ys[j] - y1) + 1n
      const area = width * height
      if (area > maxArea) maxArea = area
    }
  }

  console.log(maxArea.toString())
}

main()
