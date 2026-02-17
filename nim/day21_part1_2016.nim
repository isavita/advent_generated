
import strutils, algorithm

proc rotateRight(s: var string, n: int) =
  let steps = n mod s.len
  if steps == 0: return
  s = s[s.len - steps .. ^1] & s[0 ..< s.len - steps]

proc rotateLeft(s: var string, n: int) =
  let steps = n mod s.len
  if steps == 0: return
  s = s[steps .. ^1] & s[0 ..< steps]

proc solve() =
  var s = "abcdefgh"
  let lines = readFile("input.txt").strip().splitLines()

  for line in lines:
    let f = line.splitWhitespace()
    if f[0] == "swap":
      if f[1] == "position":
        let x = parseInt(f[2])
        let y = parseInt(f[5])
        swap(s[x], s[y])
      elif f[1] == "letter":
        let x = f[2][0]
        let y = f[5][0]
        let ix = s.find(x)
        let iy = s.find(y)
        swap(s[ix], s[iy])
    elif f[0] == "rotate":
      if f[1] == "left":
        s.rotateLeft(parseInt(f[2]))
      elif f[1] == "right":
        s.rotateRight(parseInt(f[2]))
      elif f[1] == "based":
        let i = s.find(f[6][0])
        s.rotateRight(1 + i + (if i >= 4: 1 else: 0))
    elif f[0] == "reverse":
      let x = parseInt(f[2])
      let y = parseInt(f[4])
      var i = x
      var j = y
      while i < j:
        swap(s[i], s[j])
        inc i
        dec j
    elif f[0] == "move":
      let x = parseInt(f[2])
      let y = parseInt(f[5])
      let c = s[x]
      s.delete(x .. x)
      s.insert($c, y)

  echo s

solve()

