import std/[strutils, algorithm]

proc addBig(a, b: string): string =
  var r = ""
  var i = a.len - 1
  var j = b.len - 1
  var c = 0
  while i >= 0 or j >= 0 or c != 0:
    let da = if i >= 0: ord(a[i]) - 48 else: 0
    let db = if j >= 0: ord(b[j]) - 48 else: 0
    let s = da + db + c
    r.add(chr(48 + (s mod 10)))
    c = s div 10
    i -= 1; j -= 1
  reverse(r)
  return if r == "": "0" else: r

proc mulBig(a, b: string): string =
  if a == "0" or b == "0": return "0"
  var p = newSeq[int](a.len + b.len)
  for i in countdown(a.len - 1, 0):
    for j in countdown(b.len - 1, 0):
      p[i + j + 1] += (ord(a[i]) - 48) * (ord(b[j]) - 48)
  for i in countdown(p.len - 1, 1):
    p[i - 1] += p[i] div 10
    p[i] = p[i] mod 10
  var s = ""
  var k = 0
  while k < p.len and p[k] == 0: k += 1
  while k < p.len:
    s.add(chr(48 + p[k]))
    k += 1
  return if s == "": "0" else: s

let input = try: readFile("input.txt") except: ""
let lines = input.splitLines()
var maxW = 0
for l in lines: maxW = max(maxW, l.len)

var grand = "0"
var inB = false
var sc = 0

proc procB(s, e: int) =
  var nums: seq[string] = @[]
  var op = 0
  for l in lines:
    if s < l.len:
      let seg = l[s .. min(e, l.len - 1)].strip()
      if seg == "+": op = 1
      elif seg == "*": op = 2
      elif seg != "": nums.add(seg)
  if nums.len == 0: return
  var res = "0"
  if op == 1:
    for n in nums: res = addBig(res, n)
  elif op == 2:
    res = "1"
    for n in nums: res = mulBig(res, n)
  elif nums.len == 1:
    res = nums[0]
  grand = addBig(grand, res)

for x in 0 ..< maxW:
  var sep = true
  for l in lines:
    if x < l.len and not l[x].isSpaceAscii:
      sep = false; break
  if not sep:
    if not inB:
      inB = true; sc = x
  elif inB:
    procB(sc, x - 1); inB = false
if inB: procB(sc, maxW - 1)

echo "Grand total: ", grand

