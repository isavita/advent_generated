import strutils, sequtils

let input = "input.txt".readFile().strip().splitLines()

let bits = input[0].len()
var gammaRate = ""
var epsilonRate = ""

for i in 0..<bits:
  let column = input.mapIt(it[i]).mapIt($it).toSeq
  let ones = column.count("1")
  let zeros = column.len - ones
  if ones >= zeros:
    gammaRate &= "1"
    epsilonRate &= "0"
  else:
    gammaRate &= "0"
    epsilonRate &= "1"

let gammaDecimal = parseBinInt(gammaRate)
let epsilonDecimal = parseBinInt(epsilonRate)

echo "Power consumption: ", gammaDecimal * epsilonDecimal