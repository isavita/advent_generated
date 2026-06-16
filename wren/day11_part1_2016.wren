import "io" for File

class Solver {
  construct new() {
    _n = 0
    _gen = []
    _chip = []
    _elev = 0
  }

  solve() {
    parseInput()

    var start = canonicalCode()
    var visited = {}
    visited[start] = true

    var frontier = [start]
    var steps = 0

    while (frontier.count > 0) {
      var nextFrontier = []

      for (code in frontier) {
        decode(code)

        if (done()) return steps

        var items = []
        for (i in 0..._n) {
          if (_gen[i] == _elev) items.add(i)
          if (_chip[i] == _elev) items.add(_n + i)
        }

        for (a in 0...items.count) {
          var itemA = items[a]

          if (_elev < 3) tryMove1(itemA, _elev + 1, visited, nextFrontier)
          if (_elev > 0) tryMove1(itemA, _elev - 1, visited, nextFrontier)

          for (b in (a + 1)...items.count) {
            var itemB = items[b]

            if (_elev < 3) tryMove2(itemA, itemB, _elev + 1, visited, nextFrontier)
            if (_elev > 0) tryMove2(itemA, itemB, _elev - 1, visited, nextFrontier)
          }
        }
      }

      frontier = nextFrontier
      steps = steps + 1
    }

    return -1
  }

  parseInput() {
    var text = File.read("input.txt")
    var lines = text.split("\n")
    var ids = {}
    var gen = []
    var chip = []

    for (line in lines) {
      var floor = -1

      if (line.contains("first floor")) {
        floor = 0
      } else if (line.contains("second floor")) {
        floor = 1
      } else if (line.contains("third floor")) {
        floor = 2
      } else if (line.contains("fourth floor")) {
        floor = 3
      }

      if (floor < 0) continue

      var tokens = line.split(" ")
      var current = null

      for (token in tokens) {
        var s = cleanToken(token)

        if (isSkip(s)) {
          continue
        } else if (s == "generator") {
          if (current != null) {
            var id = idFor(ids, gen, chip, current)
            gen[id] = floor
          }
        } else if (s == "microchip") {
          if (current != null) {
            var id = idFor(ids, gen, chip, current)
            chip[id] = floor
          }
        } else {
          current = s
        }
      }
    }

    _n = gen.count
    _gen = gen
    _chip = chip
  }

  cleanToken(token) {
    var s = token
    s = s.replace("-compatible", "")
    s = s.replace(".", "")
    s = s.replace(",", "")
    s = s.replace(";", "")
    s = s.replace(":", "")
    s = s.replace("\r", "")
    return s
  }

  isSkip(s) {
    if (s == "") return true
    if (s == "a") return true
    if (s == "an") return true
    if (s == "and") return true
    if (s == "the") return true
    if (s == "The") return true
    if (s == "floor") return true
    if (s == "contains") return true
    if (s == "nothing") return true
    if (s == "relevant") return true
    if (s == "compatible") return true
    if (s == "first") return true
    if (s == "second") return true
    if (s == "third") return true
    if (s == "fourth") return true
    return false
  }

  idFor(ids, gen, chip, name) {
    if (ids.containsKey(name)) return ids[name]

    var id = ids.count
    ids[name] = id
    gen.add(0)
    chip.add(0)
    return id
  }

  decode(code) {
    var t = code

    _elev = (t % 4).floor
    t = (t / 4).floor

    for (i in 0..._n) {
      _gen[i] = (t % 4).floor
      t = (t / 4).floor
    }

    for (i in 0..._n) {
      _chip[i] = (t % 4).floor
      t = (t / 4).floor
    }
  }

  canonicalCode() {
    var pairs = []

    for (i in 0..._n) {
      pairs.add(_gen[i] * 4 + _chip[i])
    }

    for (i in 1...pairs.count) {
      var v = pairs[i]
      var j = i - 1

      while (j >= 0 && pairs[j] > v) {
        pairs[j + 1] = pairs[j]
        j = j - 1
      }

      pairs[j + 1] = v
    }

    var code = _elev
    var base = 4

    for (i in 0..._n) {
      var p = pairs[i]
      code = code + base * (p / 4).floor
      base = base * 4
    }

    for (i in 0..._n) {
      var p = pairs[i]
      code = code + base * (p % 4).floor
      base = base * 4
    }

    return code
  }

  valid() {
    var g0 = 0
    var g1 = 0
    var g2 = 0
    var g3 = 0

    for (i in 0..._n) {
      if (_gen[i] == 0) {
        g0 = g0 + 1
      } else if (_gen[i] == 1) {
        g1 = g1 + 1
      } else if (_gen[i] == 2) {
        g2 = g2 + 1
      } else {
        g3 = g3 + 1
      }
    }

    for (i in 0..._n) {
      var f = _chip[i]

      if (f != _gen[i]) {
        if (f == 0 && g0 > 0) return false
        if (f == 1 && g1 > 0) return false
        if (f == 2 && g2 > 0) return false
        if (f == 3 && g3 > 0) return false
      }
    }

    return true
  }

  done() {
    for (i in 0..._n) {
      if (_gen[i] != 3) return false
      if (_chip[i] != 3) return false
    }

    return true
  }

  applyMove(item, floor) {
    if (item < _n) {
      _gen[item] = floor
    } else {
      _chip[item - _n] = floor
    }
  }

  tryMove1(item, target, visited, nextFrontier) {
    var oldElev = _elev

    applyMove(item, target)
    _elev = target

    if (valid()) {
      var nstate = canonicalCode()

      if (!visited.containsKey(nstate)) {
        visited[nstate] = true
        nextFrontier.add(nstate)
      }
    }

    _elev = oldElev
    applyMove(item, oldElev)
  }

  tryMove2(itemA, itemB, target, visited, nextFrontier) {
    var oldElev = _elev

    applyMove(itemA, target)
    applyMove(itemB, target)
    _elev = target

    if (valid()) {
      var nstate = canonicalCode()

      if (!visited.containsKey(nstate)) {
        visited[nstate] = true
        nextFrontier.add(nstate)
      }
    }

    _elev = oldElev
    applyMove(itemA, oldElev)
    applyMove(itemB, oldElev)
  }
}

var solver = Solver.new()
var result = solver.solve()

if (result == -1) {
  System.print("No solution found.")
} else {
  System.print(result)
}