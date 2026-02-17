
import std/[md5, strutils]

proc solve() =
  let doorId = readFile("input.txt").strip()
  var
    password = "        "
    found: array[8, bool]
    filled = 0
    i = 0

  while filled < 8:
    let h = getMD5(doorId & $i)
    if h[0] == '0' and h[1] == '0' and h[2] == '0' and h[3] == '0' and h[4] == '0':
      let p = h[5]
      if p >= '0' and p <= '7':
        let idx = ord(p) - ord('0')
        if not found[idx]:
          password[idx] = h[6]
          found[idx] = true
          inc filled
    inc i

  echo password

solve()

