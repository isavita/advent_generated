import strutils

const INPUT_FILE = "input.txt"

proc isRealRoom(room: string, checksum: var string): bool =
  let idx = room.find("[")
  if idx == -1: return false
  let csEnd = idx + 5
  if csEnd > room.len - 1: return false
  checksum = room[(idx + 1) .. csEnd]
  var counts: array[0..25, int] = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  for i in 0 ..< idx:
    let ch = room[i]
    if ch >= 'a' and ch <= 'z':
      counts[ord(ch) - ord('a')] += 1
  for k in 0 ..< 5:
    var maxVal = -1
    var maxIndex = -1
    for j in 0 ..< 26:
      if counts[j] > maxVal:
        maxVal = counts[j]
        maxIndex = j
    if maxIndex != ord(checksum[k]) - ord('a'):
      return false
    counts[maxIndex] = 0
  true

proc getSectorID(room: string): int =
  var lastDash = -1
  for i in 0 ..< room.len:
    if room[i] == '-':
      lastDash = i
  if lastDash == -1: return 0
  let tail = room[(lastDash + 1) .. (room.len - 1)]
  var val = 0
  for c in tail:
    if c >= '0' and c <= '9':
      val = val * 10 + (ord(c) - ord('0'))
  val

proc decryptName(room: string, sectorID: int): string =
  var res = ""
  for i in 0 ..< room.len:
    let ch = room[i]
    if ch == '-':
      res &= " "
    elif ch >= 'a' and ch <= 'z':
      let base = ord(ch) - ord('a')
      let newCode = (base + sectorID) mod 26
      let newChar = chr(ord('a') + newCode)
      res &= $newChar
    else:
      res &= $ch
  res

proc containsSub(hay: string, needle: string): bool =
  if needle.len == 0: return true
  if hay.len < needle.len: return false
  for i in 0 ..< (hay.len - needle.len + 1):
    if hay[(i) .. (i + needle.len - 1)] == needle:
      return true
  false

proc main() =
  let data = readFile(INPUT_FILE)
  for line in data.splitLines:
    var checksum = ""
    if isRealRoom(line, checksum):
      let sector = getSectorID(line)
      let decrypted = decryptName(line, sector)
      if containsSub(decrypted, "northpole object"):
        echo sector
        quit(0)

when isMainModule:
  main()