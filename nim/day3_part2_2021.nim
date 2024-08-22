import strutils, sequtils, math

proc readInput(filename: string): seq[string] =
  let file = open(filename)
  result = @[]
  for line in file.lines:
    result.add(line)
  file.close()

proc binaryToDecimal(binary: string): int =
  var decimal = 0
  var power = binary.len - 1
  for bit in binary:
    if bit == '1':
      decimal += int(math.pow(2.0, float(power)))
    dec power
  decimal

proc calculateGammaEpsilon(report: seq[string]): (int, int) =
  var gamma = ""
  var epsilon = ""
  for i in 0..<report[0].len:
    var count0 = 0
    var count1 = 0
    for number in report:
      if number[i] == '0':
        inc count0
      else:
        inc count1
    if count1 >= count0:
      gamma.add('1')
      epsilon.add('0')
    else:
      gamma.add('0')
      epsilon.add('1')
  (binaryToDecimal(gamma), binaryToDecimal(epsilon))

proc filterNumbers(report: seq[string], pos: int, bit: char): seq[string] =
  result = @[]
  for number in report:
    if number[pos] == bit:
      result.add(number)

proc calculateOxygenCO2(report: seq[string]): (int, int) =
  var oxygen = report
  var co2 = report
  var pos = 0
  while oxygen.len > 1:
    var count0 = 0
    var count1 = 0
    for number in oxygen:
      if number[pos] == '0':
        inc count0
      else:
        inc count1
    if count1 >= count0:
      oxygen = filterNumbers(oxygen, pos, '1')
    else:
      oxygen = filterNumbers(oxygen, pos, '0')
    inc pos
  pos = 0
  while co2.len > 1:
    var count0 = 0
    var count1 = 0
    for number in co2:
      if number[pos] == '0':
        inc count0
      else:
        inc count1
    if count0 <= count1:
      co2 = filterNumbers(co2, pos, '0')
    else:
      co2 = filterNumbers(co2, pos, '1')
    inc pos
  (binaryToDecimal(oxygen[0]), binaryToDecimal(co2[0]))

when isMainModule:
  let report = readInput("input.txt")
  let (gamma, epsilon) = calculateGammaEpsilon(report)
  echo "Power consumption: ", gamma * epsilon
  let (oxygen, co2) = calculateOxygenCO2(report)
  echo "Life support rating: ", oxygen * co2