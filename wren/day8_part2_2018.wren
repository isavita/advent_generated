import "io" for File

var parseTree = null

parseTree = Fn.new { |data, index|
  var childCount = data[index]
  var metaCount = data[index + 1]
  index = index + 2

  var childValues = List.new()
  for (i in 0...childCount) {
    var result = parseTree.call(data, index)
    var childValue = result[0]
    var newIndex = result[1]
    childValues.add(childValue)
    index = newIndex
  }

  var value = 0
  if (childCount == 0) {
    for (i in 0...metaCount) {
      value = value + data[index + i]
    }
  } else {
    for (i in 0...metaCount) {
      var metadata = data[index + i]
      if (metadata <= childCount && metadata > 0) {
        value = value + childValues[metadata - 1]
      }
    }
  }
  index = index + metaCount

  return [value, index]
}

var main = Fn.new {
  var input = File.read("input.txt")
  var parts = input.trim().split(" ")
  var numbers = List.new()
  for (part in parts) {
    numbers.add(Num.fromString(part))
  }

  var result = parseTree.call(numbers, 0)
  var value = result[0]
  System.print(value)
}

main.call()