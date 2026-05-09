
import "io" for File

class Coord {
  x { _x }
  y { _y }
  z { _z }
  construct new(x, y, z) {
    _x = x
    _y = y
    _z = z
  }
}

class Brick {
  id { _id }
  p1 { _p1 }
  p2 { _p2 }
  construct new(id, p1, p2) {
    _id = id
    _p1 = p1
    _p2 = p2
    if (p1.z > p2.z) {
      _p1 = p2
      _p2 = p1
    }
  }
  minZ { _p1.z.min(_p2.z) }
  maxZ { _p1.z.max(_p2.z) }
  xRange { [_p1.x.min(_p2.x), _p1.x.max(_p2.x)] }
  yRange { [_p1.y.min(_p2.y), _p1.y.max(_p2.y)] }
  overlapsXY(other) {
    var xr = xRange
    var yr = yRange
    var oxr = other.xRange
    var oyr = other.yRange
    return (xr[0].max(oxr[0]) <= xr[1].min(oxr[1])) && (yr[0].max(oyr[0]) <= yr[1].min(oyr[1]))
  }
  moveDown(dz) {
    return Brick.new(_id, Coord.new(_p1.x, _p1.y, _p1.z - dz), Coord.new(_p2.x, _p2.y, _p2.z - dz))
  }
}

var parseCoord = Fn.new { |s|
  var parts = s.split(",")
  return Coord.new(Num.fromString(parts[0]), Num.fromString(parts[1]), Num.fromString(parts[2]))
}

var parseBrick = Fn.new { |line, id|
  var ends = line.split("~")
  var p1 = parseCoord.call(ends[0])
  var p2 = parseCoord.call(ends[1])
  return Brick.new(id, p1, p2)
}

var settleBricks = Fn.new { |bricks|
  var sorted = bricks.toList
  sorted.sort { |a, b| a.minZ < b.minZ }
  var settled = []
  for (brick in sorted) {
    var current = brick
    while (current.minZ > 1) {
      var nextMinZ = current.minZ - 1
      var collision = false
      for (s in settled) {
        if (s.maxZ == nextMinZ && current.overlapsXY(s)) {
          collision = true
          break
        }
      }
      if (collision) break
      current = current.moveDown(1)
    }
    settled.add(current)
  }
  return settled
}

var buildSupportMaps = Fn.new { |settledBricks|
  var n = settledBricks.count
  var supports = (0...n).map { [] }.toList
  var supportedBy = (0...n).map { [] }.toList
  var idToIndex = List.filled(n, 0)
  for (i in 0...n) {
    idToIndex[settledBricks[i].id] = i
  }
  var sorted = settledBricks.toList
  sorted.sort { |a, b| a.minZ < b.minZ }
  for (i in 0...n) {
    for (j in i+1...n) {
      var bi = sorted[i]
      var bj = sorted[j]
      if (bj.minZ == bi.maxZ + 1 && bi.overlapsXY(bj)) {
        var idxI = idToIndex[bi.id]
        var idxJ = idToIndex[bj.id]
        supports[idxI].add(idxJ)
        supportedBy[idxJ].add(idxI)
      }
    }
  }
  return [supports, supportedBy]
}

var countSafe = Fn.new { |supports, supportedBy|
  var n = supports.count
  var safe = 0
  for (i in 0...n) {
    var essential = false
    for (j in supports[i]) {
      if (supportedBy[j].count == 1) {
        essential = true
        break
      }
    }
    if (!essential) safe = safe + 1
  }
  return safe
}

var sumChainReactions = Fn.new { |supports, supportedBy|
  var n = supports.count
  var total = 0
  for (i in 0...n) {
    var falling = {}
    falling[i] = true
    var q = []
    for (s in supports[i]) {
      q.add(s)
    }
    var head = 0
    while (head < q.count) {
      var cur = q[head]
      head = head + 1
      if (falling.containsKey(cur)) continue
      var allFallen = true
      for (sup in supportedBy[cur]) {
        if (!falling.containsKey(sup)) {
          allFallen = false
          break
        }
      }
      if (allFallen) {
        falling[cur] = true
        for (next in supports[cur]) {
          if (!falling.containsKey(next)) {
            q.add(next)
          }
        }
      }
    }
    total = total + (falling.count - 1)
  }
  return total
}

var lines = File.read("input.txt").trim().split("\n")
var bricks = []
var id = 0
for (line in lines) {
  if (line != "") {
    bricks.add(parseBrick.call(line, id))
    id = id + 1
  }
}
var settled = settleBricks.call(bricks)
var maps = buildSupportMaps.call(settled)
var supports = maps[0]
var supportedBy = maps[1]
var part1 = countSafe.call(supports, supportedBy)
var part2 = sumChainReactions.call(supports, supportedBy)
System.print("Part 1: %(part1)")
System.print("Part 2: %(part2)")
