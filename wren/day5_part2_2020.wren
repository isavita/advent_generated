import "io" for File

var binaryToInt = Fn.new { |binaryStr|
  var result = 0
  for (i in 0..binaryStr.count-1) {
    if (binaryStr[i] == "1") {
      result = result | (1 << (binaryStr.count - i - 1))
    }
  }
  return result
}

var decode = Fn.new { |pass|
  var row = binaryToInt.call(pass[0..6])
  var column = binaryToInt.call(pass[7..-1])
  return row * 8 + column
}

var main = Fn.new {
  var input = File.read("input.txt")
  var lines = input.split("\n")
  var seatIDs = []

  for (line in lines) {
    var pass = line.replace("F", "0").replace("B", "1").replace("L", "0").replace("R", "1")
    seatIDs.add(decode.call(pass))
  }

  seatIDs.sort()

  for (i in 0..seatIDs.count-2) {
    if (seatIDs[i+1] != seatIDs[i] + 1) {
      System.print(seatIDs[i] + 1)
      break
    }
  }
}

main.call()