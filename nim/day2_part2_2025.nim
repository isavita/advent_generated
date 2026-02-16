#!/usr/bin/env nim
import std/[os, strutils, tables, sets]

proc pow10(e: int): int64 =
  result = 1'i64
  for _ in 0..<e: result *= 10'i64

let txt = readFile("input.txt").replace("\r", "").replace("\n", "").strip()
if txt.len == 0:
  quit()

var ids = initHashSet[int64]()

for part in txt.split(','):
  if part.len == 0: continue
  let sp = part.split('-')
  let sStr = sp[0]
  let eStr = sp[1]
  let startNum = parseInt(sStr)
  let endNum   = parseInt(eStr)
  let sLen = sStr.len
  let eLen = eStr.len
  for totalLen in sLen..eLen:
    for k in 1..(totalLen div 2):
      if totalLen mod k != 0: continue
      let reps = totalLen div k
      let powK = pow10(k)
      let m = (pow10(k * reps) - 1) div (powK - 1)
      let minSeed = pow10(k - 1)
      let maxSeed = powK - 1
      let targetMin = (startNum + m - 1) div m
      let targetMax = endNum div m
      let seedStart = max(targetMin, minSeed)
      let seedEnd   = min(targetMax, maxSeed)
      if seedStart <= seedEnd:
        for seed in seedStart..seedEnd:
          ids.incl(seed * m)

var sum: int64 = 0
for v in ids: sum += v
echo sum