import strutils, math

proc snafuToDecimal(snafu: string): int =
  var decimal = 0
  var placeValue = 1
  for i in countdown(snafu.len - 1, 0):
    case snafu[i]
    of '2': decimal += 2 * placeValue
    of '1': decimal += 1 * placeValue
    of '0': discard
    of '-': decimal -= 1 * placeValue
    of '=': decimal -= 2 * placeValue
    else: discard
    placeValue *= 5
  decimal

proc decimalToSnafu(decimal: int): string =
  var num = decimal
  var snafu = ""
  while num != 0:
    let remainder = num mod 5
    case remainder
    of 0: snafu = "0" & snafu
    of 1: snafu = "1" & snafu
    of 2: snafu = "2" & snafu
    of 3:
      snafu = "=" & snafu
      num += 5
    of 4:
      snafu = "-" & snafu
      num += 5
    else: discard
    num = num div 5
  snafu

when isMainModule:
  let file = "input.txt"
  let lines = readFile(file).splitLines()
  var sum = 0
  for line in lines:
    sum += snafuToDecimal(line)
  echo decimalToSnafu(sum)