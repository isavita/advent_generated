import strutils, sequtils

proc incrementPassword(password: string): string =
  var passwordSeq = password.toSeq
  var carry = true
  for i in countdown(passwordSeq.high, 0):
    if carry:
      if passwordSeq[i] == 'z':
        passwordSeq[i] = 'a'
      else:
        passwordSeq[i] = chr(ord(passwordSeq[i]) + 1)
        carry = false
  result = passwordSeq.join("")

proc hasIncreasingStraight(password: string): bool =
  for i in 0..<password.len - 2:
    if ord(password[i]) + 1 == ord(password[i + 1]) and ord(password[i]) + 2 == ord(password[i + 2]):
      return true
  return false

proc hasConfusingLetters(password: string): bool =
  for c in password:
    if c in ['i', 'o', 'l']:
      return true
  return false

proc hasTwoPairs(password: string): bool =
  var pairs: seq[char] = @[]
  for i in 0..<password.len - 1:
    if password[i] == password[i + 1] and password[i] notin pairs:
      pairs.add(password[i])
    if pairs.len == 2:
      return true
  return false

proc getNextPassword(password: string): string =
  var newPassword = password
  while true:
    newPassword = incrementPassword(newPassword)
    if hasIncreasingStraight(newPassword) and not hasConfusingLetters(newPassword) and hasTwoPairs(newPassword):
      return newPassword

let input = "hxbxxyzz"
echo getNextPassword(input)