
import "io" for File

class PriorityQueue {
  construct new() {
    _heap = []
  }
  isEmpty { _heap.isEmpty }
  push(val) {
    _heap.add(val)
    up_(_heap.count - 1)
  }
  pop() {
    if (_heap.isEmpty) return null
    var top = _heap[0]
    var last = _heap.removeAt(-1)
    if (!_heap.isEmpty) {
      _heap[0] = last
      down_(0)
    }
    return top
  }
  up_(i) {
    while (i > 0) {
      var p = ((i - 1) / 2).floor
      if (_heap[p] <= _heap[i]) break
      var tmp = _heap[p]
      _heap[p] = _heap[i]
      _heap[i] = tmp
      i = p
    }
  }
  down_(i) {
    var len = _heap.count
    while (2 * i + 1 < len) {
      var left = 2 * i + 1
      var right = left + 1
      var best = left
      if (right < len && _heap[right] < _heap[left]) {
        best = right
      }
      if (_heap[i] <= _heap[best]) break
      var tmp = _heap[i]
      _heap[i] = _heap[best]
      _heap[best] = tmp
      i = best
    }
  }
}

var lines = File.read("input.txt").replace("\r", "").split("\n").where { |l| l.count > 0 }.toList
var H = lines.count
var W = lines[0].count

var startState = -1
var endR = -1
var endC = -1

var grid = List.filled(H * W, false)

for (r in 0...H) {
  var line = lines[r]
  for (c in 0...W) {
    var char = line[c]
    if (char == "#") {
      grid[r * W + c] = true
    } else if (char == "S") {
      startState = (r * W + c) * 4 + 1
    } else if (char == "E") {
      endR = r
      endC = c
    }
  }
}

var dr = [-1, 0, 1, 0]
var dc = [0, 1, 0, -1]

var pq = PriorityQueue.new()
pq.push(0 * 1000000 + startState)

var INF = 999999999
var dist = List.filled(H * W * 4, INF)
dist[startState] = 0

while (!pq.isEmpty) {
  var curr = pq.pop()
  var cost = (curr / 1000000).floor
  var state = curr % 1000000
  
  if (cost > dist[state]) continue
  
  var d = state % 4
  var rc = (state / 4).floor
  var c = rc % W
  var r = (rc / W).floor
  
  for (turn in [-1, 1]) {
    var nd = (d + turn + 4) % 4
    var nstate = rc * 4 + nd
    var ncost = cost + 1000
    if (ncost < dist[nstate]) {
      dist[nstate] = ncost
      pq.push(ncost * 1000000 + nstate)
    }
  }
  
  var nr = r + dr[d]
  var nc = c + dc[d]
  if (nr >= 0 && nr < H && nc >= 0 && nc < W) {
    var idx = nr * W + nc
    if (!grid[idx]) {
      var nstate = idx * 4 + d
      var ncost = cost + 1
      if (ncost < dist[nstate]) {
        dist[nstate] = ncost
        pq.push(ncost * 1000000 + nstate)
      }
    }
  }
}

var best = INF
for (d in 0...4) {
  var state = (endR * W + endC) * 4 + d
  if (dist[state] < best) {
    best = dist[state]
  }
}

var rev_q = []
var vis = List.filled(H * W * 4, false)

for (d in 0...4) {
  var state = (endR * W + endC) * 4 + d
  if (dist[state] == best) {
    vis[state] = true
    rev_q.add(state)
  }
}

var head = 0
var used = List.filled(H * W, false)

while (head < rev_q.count) {
  var state = rev_q[head]
  head = head + 1
  
  var d = state % 4
  var rc = (state / 4).floor
  var c = rc % W
  var r = (rc / W).floor
  
  used[rc] = true
  var costU = dist[state]
  
  for (turn in [-1, 1]) {
    var pd = (d + turn + 4) % 4
    var prev_state = rc * 4 + pd
    if (dist[prev_state] == costU - 1000) {
      if (!vis[prev_state]) {
        vis[prev_state] = true
        rev_q.add(prev_state)
      }
    }
  }
  
  var pr = r - dr[d]
  var pc = c - dc[d]
  if (pr >= 0 && pr < H && pc >= 0 && pc < W) {
    var p_idx = pr * W + pc
    if (!grid[p_idx]) {
      var prev_state = p_idx * 4 + d
      if (dist[prev_state] == costU - 1) {
        if (!vis[prev_state]) {
          vis[prev_state] = true
          rev_q.add(prev_state)
        }
      }
    }
  }
}

var count = 0
for (val in used) {
  if (val) count = count + 1
}
System.print(count)
