
import "io" for File

var increment = Fn.new { |bytes|
  var i = bytes.count - 1
  while (i >= 0) {
    if (bytes[i] == 122) {
      bytes[i] = 97
      i = i - 1
    } else {
      bytes[i] = bytes[i] + 1
      break
    }
  }
}

var isValid = Fn.new { |bytes|
  var hasStraight = false
  var i = 0
  while (i < bytes.count - 2) {
    if (bytes[i+1] == bytes[i] + 1 && bytes[i+2] == bytes[i] + 2) {
      hasStraight = true
      break
    }
    i = i + 1
  }
  if (!hasStraight) return false

  for (b in bytes) {
    if (b == 105 || b == 111 || b == 108) return false
  }

  var pairs = 0
  i = 0
  while (i < bytes.count - 1) {
    if (bytes[i] == bytes[i+1]) {
      pairs = pairs + 1
      i = i + 2
    } else {
      i = i + 1
    }
  }
  return pairs >= 2
}

var password = File.read("input.txt").trim()
var bytes = password.bytes.toList

while (true) {
  increment.call(bytes)
  if (isValid.call(bytes)) break
}

System.print(bytes.map { |b| String.fromByte(b) }.join(""))
