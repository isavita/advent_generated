import os

proc toInt64(s: string): int64 =
  var res: int64 = 0
  for ch in s:
    res = res * 10 + int64(ord(ch) - ord('0'))
  return res

proc modPow(base: int64, exp: int64, modv: int64): int64 =
  var b = base mod modv
  var e = exp
  var r: int64 = 1
  while e > 0:
    if (e and 1) != 0:
      r = (r * b) mod modv
    b = (b * b) mod modv
    e = e shr 1
  return r

proc main() =
  let content = readFile("input.txt")
  var tokens: seq[string] = @[]
  var cur = ""
  for ch in content:
    if ch >= '0' and ch <= '9':
      cur &= ch
    else:
      if cur.len > 0:
        tokens.add(cur)
        cur = ""
  if cur.len > 0:
    tokens.add(cur)

  if tokens.len < 2:
    quit(1)

  let row = toInt64(tokens[0])
  let column = toInt64(tokens[1])

  let sum = row + column
  let pos: int64 = ((sum - 2) * (sum - 1)) div 2 + column

  const initialCode: int64 = 20151125
  const mult: int64 = 252533
  const modv: int64 = 33554393

  let powPart = modPow(mult, pos - 1, modv)
  let code = (initialCode * powPart) mod modv
  echo code

when isMainModule:
  main()