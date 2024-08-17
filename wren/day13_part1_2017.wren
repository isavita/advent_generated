import "io" for File

class Scanner {
  construct new(range, position, direction) {
    _range = range
    _position = position
    _direction = direction
  }

  range { _range }
  position { _position }
  direction { _direction }

  move() {
    if (_position == 0) {
      _direction = 1
    } else if (_position == _range - 1) {
      _direction = -1
    }
    _position = _position + _direction
  }
}

var main = Fn.new {
  var input = File.read("input.txt")
  var lines = input.split("\n")
  var firewall = {}

  for (line in lines) {
    var fields = line.split(": ")
    var depth = Num.fromString(fields[0])
    var rng = Num.fromString(fields[1])
    firewall[depth] = Scanner.new(rng, 0, 1)
  }

  var severity = 0
  var maxDepth = 0
  for (depth in firewall.keys) {
    if (depth > maxDepth) maxDepth = depth
  }

  for (depth in 0..maxDepth) {
    if (firewall.containsKey(depth)) {
      var scanner = firewall[depth]
      if (scanner.position == 0) {
        severity = severity + (depth * scanner.range)
      }
    }

    for (scanner in firewall.values) {
      scanner.move()
    }
  }

  System.print(severity)
}

main.call()