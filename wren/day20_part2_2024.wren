
import "io" for File

class Point {
  construct new(x, y) {
    _x = x
    _y = y
  }
  x { _x }
  y { _y }
}

var h = null
var w = null
var S = null
var E = null
var walls = null
var trackCells = []
var dirs = [Point.new(1, 0), Point.new(-1, 0), Point.new(0, 1), Point.new(0, -1)]

var isTrack = Fn.new { |x, y|
  return x >= 0 && x < h && y >= 0 && y < w && !walls[x][y]
}

var normalDistFrom = Fn.new { |start|
  var dist = List.filled(h, null)
  for (i in 0...h) {
    dist[i] = List.filled(w, -1)
  }
  dist[start.x][start.y] = 0
  var q = [start]
  while (q.count > 0) {
    var cur = q.removeAt(0)
    for (d in dirs) {
      var nx = cur.x + d.x
      var ny = cur.y + d.y
      if (nx < 0 || nx >= h || ny < 0 || ny >= w) continue
      if (walls[nx][ny]) continue
      if (dist[nx][ny] < 0) {
        dist[nx][ny] = dist[cur.x][cur.y] + 1
        q.add(Point.new(nx, ny))
      }
    }
  }
  return dist
}

var lines = File.read("input.txt").split("\n")
h = lines.count
w = lines[0].count
walls = List.filled(h, null)
for (i in 0...h) {
  walls[i] = List.filled(w, false)
}

for (i in 0...h) {
  var line = lines[i]
  for (j in 0...w) {
    var ch = line[j]
    if (ch == "S") {
      S = Point.new(i, j)
    } else if (ch == "E") {
      E = Point.new(i, j)
    }
    if (ch == "#") {
      walls[i][j] = true
    } else {
      trackCells.add(Point.new(i, j))
    }
  }
}

var distFromS = normalDistFrom.call(S)
var distFromE = normalDistFrom.call(E)
var normalCost = distFromS[E.x][E.y]
if (normalCost < 0) {
  System.print(0)
  return
}

var cheats = {}
for (startPos in trackCells) {
  var sd = distFromS[startPos.x][startPos.y]
  if (sd < 0) continue

  var distC = List.filled(h, null)
  for (i in 0...h) {
    distC[i] = List.filled(w, -1)
  }
  distC[startPos.x][startPos.y] = 0
  var q = [startPos]

  while (q.count > 0) {
    var cur = q.removeAt(0)
    var steps = distC[cur.x][cur.y]
    if (steps == 20) continue
    for (d in dirs) {
      var nx = cur.x + d.x
      var ny = cur.y + d.y
      if (nx < 0 || nx >= h || ny < 0 || ny >= w) continue
      if (distC[nx][ny] < 0) {
        distC[nx][ny] = steps + 1
        q.add(Point.new(nx, ny))
      }
    }
  }

  for (x in 0...h) {
    for (y in 0...w) {
      var s = distC[x][y]
      if (s > 0 && s <= 20 && isTrack.call(x, y)) {
        var ed = distFromE[x][y]
        if (ed < 0) continue
        var cost = sd + s + ed
        if (cost < normalCost) {
          var key = "%(startPos.x),%(startPos.y),%(x),%(y)"
          if (!cheats.containsKey(key) || cost < cheats[key]) {
            cheats[key] = cost
          }
        }
      }
    }
  }
}

var count = 0
for (cost in cheats.values) {
  var saving = normalCost - cost
  if (saving >= 100) {
    count = count + 1
  }
}
System.print(count)
