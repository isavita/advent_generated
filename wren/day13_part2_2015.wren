import "io" for File

class Guest {
  construct new(name) {
    _name = name
    _happiness = []
  }

  construct withRow(name, row) {
    _name = name
    _happiness = row
  }

  happiness { _happiness }

  setHappiness(index, value) {
    _happiness[index] = value
  }

  getHappiness(index) {
    return _happiness[index]
  }
}

class Solver {
  construct new() {
    _guestIndex = {}
    _guests = []
    _pair = []
  }

  ensureGuest(name) {
    var index = _guestIndex[name]
    if (index != null) return index

    index = _guests.count
    _guestIndex[name] = index

    var count = _guests.count + 1
    for (guest in _guests) guest.happiness.add(0)

    _guests.add(Guest.withRow(name, List.filled(count, 0)))

    return index
  }

  readInput() {
    var text = File.read("input.txt")
    for (rawLine in text.split("\n")) {
      var line = rawLine.trim()
      if (line.count > 0) {
        var parts = line.split(" ")
        var from = parts[0]
        var action = parts[2]
        var change = Num.fromString(parts[3])
        var to = parts[10]

        if (to.endsWith(".")) {
          var end = to.count - 1
          to = to[0...end]
        }

        if (action == "lose") change = -change

        var fromIndex = ensureGuest(from)
        var toIndex = ensureGuest(to)
        _guests[fromIndex].setHappiness(toIndex, change)
      }
    }
  }

  buildPairs() {
    var n = _guests.count
    _pair = List.filled(n, null)

    for (i in 0...n) {
      _pair[i] = List.filled(n, 0)
      for (j in 0...n) {
        _pair[i][j] = _guests[i].getHappiness(j) + _guests[j].getHappiness(i)
      }
    }
  }

  run() {
    ensureGuest("You")
    readInput()
    buildPairs()

    var m = _guests.count - 1
    if (m == 0) {
      System.print(0)
      return
    }

    var starts = []
    var ends = []
    var edges = List.filled(m, null)

    for (v in 0...m) {
      starts.add(_pair[0][v + 1])
      ends.add(_pair[v + 1][0])
      edges[v] = List.filled(m, 0)
      for (e in 0...m) {
        edges[v][e] = _pair[v + 1][e + 1]
      }
    }

    var prev = {}
    for (v in 0...m) {
      var row = List.filled(m, null)
      row[v] = starts[v]
      prev[1 << v] = row
    }

    if (m >= 2) {
      for (size in 2..m) {
        var current = {}

        for (entry in prev) {
          var mask = entry.key
          var values = entry.value

          for (e in 0...m) {
            if ((mask & (1 << e)) == 0) {
              var newMask = mask | (1 << e)
              var best = null

              for (p in 0...m) {
                if ((mask & (1 << p)) != 0) {
                  var value = values[p]
                  if (value != null) {
                    var candidate = value + edges[p][e]
                    if (best == null) {
                      best = candidate
                    } else if (candidate > best) {
                      best = candidate
                    }
                  }
                }
              }

              if (best != null) {
                var row = current[newMask]
                if (row == null) {
                  row = List.filled(m, null)
                  current[newMask] = row
                }
                row[e] = best
              }
            }
          }
        }

        prev = current
      }
    }

    var allMask = (1 << m) - 1
    var finalRow = prev[allMask]
    var best = null

    for (e in 0...m) {
      var value = finalRow[e]
      if (value != null) {
        var candidate = value + ends[e]
        if (best == null) {
          best = candidate
        } else if (candidate > best) {
          best = candidate
        }
      }
    }

    System.print(best)
  }
}

Solver.new().run()