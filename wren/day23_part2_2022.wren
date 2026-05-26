
import "io" for File

var map = {}
var lines = File.read("input.txt").replace("\r", "").split("\n")
var offset = 10000000
var mult = 20000000

var row = 0
for (line in lines) {
  if (line.bytes.count > 0) {
    for (col in 0...line.count) {
      if (line[col] == "#") {
        var key = (row + offset) * mult + (col + offset)
        map[key] = true
      }
    }
    row = row + 1
  }
}

var dx = [-1, -1, -1, 0, 1, 1, 1, 0]
var dy = [-1, 0, 1, 1, 1, 0, -1, -1]
var order = [1, 5, 7, 3]

var dir_offsets = List.filled(8, 0)
for (i in 0..7) {
  dir_offsets[i] = dx[i] * mult + dy[i]
}

var round = 0
var curr_dir_idx = 0

while (true) {
  var proposes = {}
  var next_pos = {}
  var someone_moved = false

  for (key in map.keys) {
    var alone = true
    for (i in 0..7) {
      if (map.containsKey(key + dir_offsets[i])) {
        alone = false
        break
      }
    }
    if (alone) continue

    for (j in 0..3) {
      var idx = curr_dir_idx + j
      if (idx >= 4) idx = idx - 4
      var dir_idx = order[idx]

      var can_propose = true
      for (k in -1..1) {
        var check_idx = dir_idx + k
        if (check_idx == 8) check_idx = 0
        if (map.containsKey(key + dir_offsets[check_idx])) {
          can_propose = false
          break
        }
      }

      if (can_propose) {
        var dest_key = key + dir_offsets[dir_idx]
        next_pos[key] = dest_key
        var count = proposes[dest_key]
        if (count == null) {
          proposes[dest_key] = 1
        } else {
          proposes[dest_key] = count + 1
        }
        break
      }
    }
  }

  for (key in next_pos.keys) {
    var dest_key = next_pos[key]
    if (proposes[dest_key] == 1) {
      map.remove(key)
      map[dest_key] = true
      someone_moved = true
    }
  }

  if (!someone_moved) {
    System.print(round + 1)
    break
  }

  round = round + 1
  curr_dir_idx = curr_dir_idx + 1
  if (curr_dir_idx == 4) curr_dir_idx = 0
}
