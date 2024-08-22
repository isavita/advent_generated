import strutils, sequtils

proc supportsTLS(ip: string): bool =
  var insideBrackets = false
  var hasABBA = false
  for i in 0..<ip.len-3:
    if ip[i] == '[':
      insideBrackets = true
      continue
    elif ip[i] == ']':
      insideBrackets = false
      continue
    if ip[i] == ip[i+3] and ip[i+1] == ip[i+2] and ip[i] != ip[i+1]:
      if insideBrackets:
        return false
      hasABBA = true
  return hasABBA

proc supportsSSL(ip: string): bool =
  var supernet, hypernet: seq[string]
  var insideBrackets = false
  var current = ""
  for c in ip:
    if c == '[':
      if not insideBrackets:
        supernet.add(current)
      else:
        hypernet.add(current)
      current = ""
      insideBrackets = not insideBrackets
    elif c == ']':
      if insideBrackets:
        hypernet.add(current)
      else:
        supernet.add(current)
      current = ""
      insideBrackets = not insideBrackets
    else:
      current.add(c)
  if insideBrackets:
    hypernet.add(current)
  else:
    supernet.add(current)

  for s in supernet:
    for i in 0..<s.len-2:
      if s[i] == s[i+2] and s[i] != s[i+1]:
        let aba = s[i..i+2]
        let bab = $aba[1] & $aba[0] & $aba[1]
        for h in hypernet:
          if h.contains(bab):
            return true
  return false

let input = readFile("input.txt").splitLines()
var tlsCount, sslCount = 0
for ip in input:
  if supportsTLS(ip):
    tlsCount.inc
  if supportsSSL(ip):
    sslCount.inc

echo "TLS count: ", tlsCount
echo "SSL count: ", sslCount