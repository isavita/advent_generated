
import "io" for File

class Program {
  static run() {
    var content = File.read("input.txt")
    var lines = content.split("\n")
    var xs = []
    var ys = []
    for (line in lines) {
      var trimmed = line.trim()
      if (trimmed.bytes.count > 0) {
        var parts = trimmed.split(",")
        xs.add(Num.fromString(parts[0]))
        ys.add(Num.fromString(parts[1]))
      }
    }
    var n = xs.count

    var maxX = 70
    var maxY = 70
    var size = 71 * 71

    var bfs = Fn.new { |k|
      var blocked = List.filled(size, false)
      for (i in 0..k) {
        blocked[xs[i] + ys[i] * 71] = true
      }
      
      var visited = List.filled(size, -1)
      var q = List.filled(size, 0)
      var head = 0
      var tail = 0
      
      q[tail] = 0
      tail = tail + 1
      visited[0] = 0
      
      var dirs = [1, 0, -1, 0, 0, 1, 0, -1]
      
      while (head < tail) {
        var curr = q[head]
        head = head + 1
        var cx = curr % 71
        var cy = (curr / 71).floor
        var d = visited[curr]
        
        if (cx == maxX && cy == maxY) return d
        
        for (i in 0...4) {
          var nx = cx + dirs[i * 2]
          var ny = cy + dirs[i * 2 + 1]
          if (nx >= 0 && nx <= maxX && ny >= 0 && ny <= maxY) {
            var nidx = nx + ny * 71
            if (!blocked[nidx] && visited[nidx] == -1) {
              visited[nidx] = d + 1
              q[tail] = nidx
              tail = tail + 1
            }
          }
        }
      }
      return -1
    }

    var limit = (n < 1024 ? n : 1024) - 1
    System.print(bfs.call(limit))

    var low = limit + 1
    var high = n - 1
    while (low < high) {
      var mid = ((low + high) / 2).floor
      if (bfs.call(mid) != -1) {
        low = mid + 1
      } else {
        high = mid
      }
    }
    System.print("%(xs[low]),%(ys[low])")
  }
}

Program.run()
